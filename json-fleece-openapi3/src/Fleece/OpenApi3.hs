{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3
  ( generateOpenApiFleeceCode
  ) where

import Control.Monad (join, (<=<))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.OpenApi as OA
import qualified Data.Text as T

import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.HaskellCode as HC

generateOpenApiFleeceCode ::
  OA.OpenApi ->
  CGU.CodeGen CGU.Modules
generateOpenApiFleeceCode swagger = do
  typeMap <- mkCodeGenTypes swagger
  CGU.generateFleeceCode typeMap

type SchemaMap =
  Map.Map T.Text SchemaEntry

data SchemaEntry = SchemaEntry
  { schemaCodeGenType :: CGU.CodeGenType
  , schemaOpenApiSchema :: OA.Schema
  }

mkCodeGenTypes :: OA.OpenApi -> CGU.CodeGen CGU.TypeMap
mkCodeGenTypes openApi = do
  schemaMap <-
    fmap Map.unions
      . traverse (uncurry mkSchemaMap)
      . IOHM.toList
      . OA._componentsSchemas
      . OA._openApiComponents
      $ openApi

  let
    pathItems =
      IOHM.toList
        . OA._openApiPaths
        $ openApi

    typeMap =
      fmap (CGU.CodeGenItemType . schemaCodeGenType) schemaMap

  pathTypes <- traverse (uncurry $ mkPathItem schemaMap) pathItems
  pure $ Map.unions (typeMap : pathTypes)

mkPathItem :: SchemaMap -> FilePath -> OA.PathItem -> CGU.CodeGen CGU.TypeMap
mkPathItem schemaMap filePath pathItem =
  fmap Map.unions $
    traverse
      (uncurry $ mkOperation schemaMap filePath pathItem)
      (pathItemOperations pathItem)

pathItemOperations :: OA.PathItem -> [(T.Text, OA.Operation)]
pathItemOperations pathItem =
  let
    mkItem (method, accessor) =
      case accessor pathItem of
        Nothing -> Nothing
        Just operation -> Just (method, operation)
  in
    mapMaybe
      mkItem
      [ ("GET", OA._pathItemGet)
      , ("PUT", OA._pathItemPut)
      , ("POST", OA._pathItemPost)
      , ("DELETE", OA._pathItemDelete)
      , ("OPTIONS", OA._pathItemOptions)
      , ("HEAD", OA._pathItemHead)
      , ("PATCH", OA._pathItemPatch)
      , ("TRACE", OA._pathItemTrace)
      ]

mkOperation ::
  SchemaMap ->
  FilePath ->
  OA.PathItem ->
  T.Text ->
  OA.Operation ->
  CGU.CodeGen CGU.TypeMap
mkOperation schemaMap filePath pathItem method operation = do
  let
    pathTextParts =
      filter (not . T.null)
        . T.splitOn "/"
        . T.pack
        $ filePath

    operationKey =
      case OA._operationOperationId operation of
        Just operationId -> operationId
        Nothing -> T.intercalate "." pathTextParts

  params <-
    mkOperationParams schemaMap operationKey pathItem operation

  let
    lookupParamRef name =
      case Map.lookup name params of
        Just codeGenParam ->
          pure $
            CGU.PathParamRef
              (CGU.codeGenOperationParamName codeGenParam)
              (CGU.codeGenOperationParamTypeName codeGenParam)
              (CGU.codeGenOperationParamDefName codeGenParam)
        Nothing ->
          CGU.codeGenError $
            "Parameter definition not found for "
              <> show name
              <> " param of "
              <> show method
              <> " operation for "
              <> filePath

    mkPiece text =
      if "{" `T.isPrefixOf` text && "}" `T.isSuffixOf` text
        then lookupParamRef (T.drop 1 . T.dropEnd 1 $ text)
        else pure (CGU.PathLiteral text)

  pathPieces <- traverse mkPiece pathTextParts

  mbRequestBody <- lookupRequestBody operationKey operation

  let
    mbJSONMedia =
      IOHM.lookup "application/json"
        . OA._requestBodyContent
        =<< mbRequestBody

  mbRequestBodySchema <-
    fmap join
      . traverse (lookupRequestBodySchema operationKey schemaMap)
      $ mbJSONMedia

  responses <-
    lookupResponses
      operationKey
      schemaMap
      (OA._operationResponses operation)

  let
    codeGenOperation =
      CGU.CodeGenOperation
        { CGU.codeGenOperationOriginalName = operationKey
        , CGU.codeGenOperationMethod = method
        , CGU.codeGenOperationPath = pathPieces
        , CGU.codeGenOperationParams = Map.elems params
        , CGU.codeGenOperationRequestBody = fmap schemaCodeGenType mbRequestBodySchema
        , CGU.codeGenOperationResponses = responses
        }

    mkParamEntry (paramName, param) =
      ( operationKey <> "." <> paramName
      , CGU.CodeGenItemOperationParam param
      )

    paramModules =
      Map.fromList
        . map mkParamEntry
        . Map.toList
        $ params

  pure $
    Map.singleton operationKey (CGU.CodeGenItemOperation codeGenOperation)
      <> paramModules

lookupRequestBody ::
  T.Text ->
  OA.Operation ->
  CGU.CodeGen (Maybe OA.RequestBody)
lookupRequestBody operationKey operation =
  case OA._operationRequestBody operation of
    Just (OA.Ref _reference) ->
      CGU.codeGenError $
        "Error finding request body for operation "
          <> show operationKey
          <> ": request body references are not currently supported."
    Just (OA.Inline body) ->
      pure (Just body)
    Nothing ->
      pure Nothing

lookupRequestBodySchema ::
  T.Text ->
  SchemaMap ->
  OA.MediaTypeObject ->
  CGU.CodeGen (Maybe SchemaEntry)
lookupRequestBodySchema operationKey schemaMap mediaTypeObject =
  case OA._mediaTypeObjectSchema mediaTypeObject of
    Just (OA.Ref (OA.Reference refKey)) ->
      case Map.lookup refKey schemaMap of
        Just schemaEntry -> pure (Just schemaEntry)
        Nothing ->
          CGU.codeGenError $
            "Error finding request body schema for operation "
              <> show operationKey
              <> ": unable to resolve schema reference "
              <> show refKey
              <> "."
    Just (OA.Inline _schema) ->
      CGU.codeGenError $
        "Error finding request body schema for operation "
          <> show operationKey
          <> ": inline request body schemas are not currently supported."
    Nothing ->
      pure Nothing

lookupResponses ::
  T.Text ->
  SchemaMap ->
  OA.Responses ->
  CGU.CodeGen (Map.Map CGU.ResponseStatus (Maybe CGU.SchemaTypeInfo))
lookupResponses operationKey schemaMap responses =
  let
    statusCodeEntries =
      Map.fromList
        . map (\(status, responseRef) -> (CGU.ResponseStatusCode status, responseRef))
        . IOHM.toList
        . OA._responsesResponses
        $ responses

    allEntries =
      case OA._responsesDefault responses of
        Just defaultResponseRef ->
          Map.insert CGU.DefaultResponse defaultResponseRef statusCodeEntries
        Nothing -> statusCodeEntries
  in
    traverse
      (lookupResponse operationKey schemaMap)
      allEntries

lookupResponse ::
  T.Text ->
  SchemaMap ->
  OA.Referenced OA.Response ->
  CGU.CodeGen (Maybe CGU.SchemaTypeInfo)
lookupResponse operationKey schemaMap responseRef =
  let
    responseError msg =
      CGU.codeGenError $
        "Error looking up response for operation "
          <> show operationKey
          <> ": "
          <> msg

    lookupCodeGenType refKey =
      case Map.lookup refKey schemaMap of
        Just schemaEntry ->
          pure . CGU.codeGenTypeSchemaInfo . schemaCodeGenType $ schemaEntry
        Nothing ->
          responseError $
            "Unable to resolve schema reference "
              <> show refKey
              <> "."
  in
    case responseRef of
      OA.Ref _reference ->
        responseError "Response references are not yet supported."
      OA.Inline response ->
        case IOHM.lookup "application/json" (OA._responseContent response) of
          Nothing -> pure Nothing
          Just mediaTypeObject ->
            case OA._mediaTypeObjectSchema mediaTypeObject of
              Just (OA.Ref (OA.Reference refKey)) ->
                fmap Just (lookupCodeGenType refKey)
              Just (OA.Inline schema) ->
                case OA._schemaType schema of
                  Just OA.OpenApiArray ->
                    case OA._schemaItems schema of
                      Just (OA.OpenApiItemsObject (OA.Ref (OA.Reference itemRefKey))) -> do
                        itemSchemaInfo <- lookupCodeGenType itemRefKey
                        pure . Just . CGU.arrayTypeInfo $ itemSchemaInfo
                      Just (OA.OpenApiItemsObject (OA.Inline _schema)) -> do
                        responseError $
                          "Inline schemas for array items are not yet supported."
                      otherItemType ->
                        responseError $
                          "Unsupported schema array item type found: "
                            <> show otherItemType
                  _ ->
                    responseError "Inline response schemas are not currently supported (except for arrays)."
              Nothing ->
                pure Nothing

mkOperationParams ::
  SchemaMap ->
  T.Text ->
  OA.PathItem ->
  OA.Operation ->
  CGU.CodeGen (Map.Map T.Text CGU.CodeGenOperationParam)
mkOperationParams schemaMap operationKey pathItem operation = do
  paramList <-
    traverse
      (mkOperationParam schemaMap operationKey)
      (OA._pathItemParameters pathItem <> OA._operationParameters operation)

  let
    paramMap =
      Map.fromList
        . map (\param -> (CGU.codeGenOperationParamName param, param))
        $ paramList

  pure paramMap

mkOperationParam ::
  SchemaMap ->
  T.Text ->
  OA.Referenced OA.Param ->
  CGU.CodeGen CGU.CodeGenOperationParam
mkOperationParam schemaMap operationKey paramRef = do
  param <-
    case paramRef of
      OA.Ref _ -> CGU.codeGenError "Param refs not yet implemeted."
      OA.Inline param -> pure param

  let
    paramName =
      OA._paramName param

  (moduleName, defaultParamTypeName) <-
    CGU.inferTypeForInputName CGU.Operation (operationKey <> "." <> paramName)

  case OA._paramSchema param of
    Just schemaRef -> do
      paramInfo <-
        schemaRefToParamInfo
          schemaMap
          paramName
          (OA._paramIn param)
          operationKey
          schemaRef

      let
        paramTypeName =
          case paramInfoTypeName paramInfo of
            Nothing -> defaultParamTypeName
            Just resolvedName -> resolvedName

        paramRequired =
          case OA._paramRequired param of
            Nothing -> False
            Just req -> req

        arity =
          case (paramRequired, paramInfoArray paramInfo) of
            (True, False) -> CGU.ExactlyOne
            (False, False) -> CGU.AtMostOne
            (True, True) -> CGU.AtLeastOne
            (False, True) -> CGU.AtLeastZero

      paramLocation <-
        case OA._paramIn param of
          OA.ParamQuery -> pure CGU.ParamLocationQuery
          OA.ParamPath -> pure CGU.ParamLocationPath
          OA.ParamHeader -> pure CGU.ParamLocationHeader
          OA.ParamCookie -> paramCodeGenError paramName operationKey "Cookie params not supported."

      pure
        CGU.CodeGenOperationParam
          { CGU.codeGenOperationParamName = paramName
          , CGU.codeGenOperationParamArity = arity
          , CGU.codeGenOperationParamModuleName = moduleName
          , CGU.codeGenOperationParamTypeName = paramTypeName
          , CGU.codeGenOperationParamFormat = paramInfoFormat paramInfo
          , CGU.codeGenOperationParamLocation = paramLocation
          , CGU.codeGenOperationParamDefName =
              HC.toVarName
                moduleName
                (Just (HC.typeNameText paramTypeName))
                "paramDef"
          }
    Nothing ->
      paramCodeGenError paramName operationKey "No param schema found."

paramCodeGenError :: T.Text -> T.Text -> String -> CGU.CodeGen a
paramCodeGenError paramName operationKey msg =
  CGU.codeGenError $
    "Error handing param "
      <> T.unpack paramName
      <> " of operation "
      <> T.unpack operationKey
      <> ": "
      <> msg

data ParamInfo = ParamInfo
  { paramInfoTypeName :: Maybe HC.TypeName
  , paramInfoArray :: Bool
  , paramInfoFormat :: CGU.OperationParamFormat
  }

primitiveParamInfo :: CGU.OperationParamFormat -> ParamInfo
primitiveParamInfo format =
  ParamInfo
    { paramInfoTypeName = Nothing
    , paramInfoArray = False
    , paramInfoFormat = format
    }

schemaRefToParamInfo ::
  SchemaMap ->
  T.Text ->
  OA.ParamLocation ->
  T.Text ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen ParamInfo
schemaRefToParamInfo schemaMap paramName paramLocation operationKey schemaRef =
  case schemaRef of
    OA.Inline schema -> do
      schemaTypeToParamInfo
        schemaMap
        paramName
        paramLocation
        operationKey
        schema
    OA.Ref (OA.Reference refKey) ->
      case Map.lookup refKey schemaMap of
        Just schemaEntry -> do
          let
            codeGenType =
              schemaCodeGenType schemaEntry

          paramInfo <-
            schemaTypeToParamInfo
              schemaMap
              paramName
              paramLocation
              operationKey
              (schemaOpenApiSchema schemaEntry)

          pure $
            paramInfo
              { paramInfoTypeName = Just (CGU.codeGenTypeName codeGenType)
              }
        Nothing ->
          paramCodeGenError paramName operationKey $
            "Schema reference "
              <> show refKey
              <> " not found."

schemaTypeToParamInfo ::
  SchemaMap ->
  T.Text ->
  OA.ParamLocation ->
  T.Text ->
  OA.Schema ->
  CGU.CodeGen ParamInfo
schemaTypeToParamInfo schemaMap paramName paramLocation operationKey schema =
  case OA._schemaType schema of
    Just OA.OpenApiString ->
      case OA._schemaEnum schema of
        Nothing ->
          pure (primitiveParamInfo CGU.ParamTypeString)
        Just enumValues -> do
          let
            rejectNull mbText =
              case mbText of
                Nothing -> CGU.codeGenError "null not supported as enum value in params"
                Just text -> pure text

          enumTexts <-
            traverse (rejectNull <=< enumValueToText schema) enumValues

          pure
            . primitiveParamInfo
            . CGU.ParamTypeEnum
            $ enumTexts
    Just OA.OpenApiBoolean ->
      pure (primitiveParamInfo CGU.ParamTypeBoolean)
    Just OA.OpenApiInteger ->
      case OA._schemaFormat schema of
        Just "int8" -> pure (primitiveParamInfo CGU.ParamTypeInt8)
        Just "int16" -> pure (primitiveParamInfo CGU.ParamTypeInt16)
        Just "int32" -> pure (primitiveParamInfo CGU.ParamTypeInt32)
        Just "int64" -> pure (primitiveParamInfo CGU.ParamTypeInt64)
        _ -> pure (primitiveParamInfo CGU.ParamTypeInteger)
    Just OA.OpenApiNumber ->
      case OA._schemaFormat schema of
        Just "double" -> pure (primitiveParamInfo CGU.ParamTypeDouble)
        Just "float" -> pure (primitiveParamInfo CGU.ParamTypeFloat)
        _ -> pure (primitiveParamInfo CGU.ParamTypeScientific)
    Just OA.OpenApiArray ->
      case paramLocation of
        OA.ParamQuery ->
          case OA._schemaItems schema of
            Just (OA.OpenApiItemsObject itemSchemaRef) -> do
              itemInfo <-
                schemaRefToParamInfo
                  schemaMap
                  paramName
                  paramLocation
                  operationKey
                  itemSchemaRef

              if paramInfoArray itemInfo
                then
                  paramCodeGenError
                    paramName
                    operationKey
                    "Array of arrays not support for param"
                else
                  pure $
                    itemInfo
                      { paramInfoArray = True
                      }
            otherItemType ->
              paramCodeGenError paramName operationKey $
                "Unsupported schema array item type found: "
                  <> show otherItemType
        otherLocation ->
          paramCodeGenError paramName operationKey $
            "Array parameters are not supported for "
              <> show otherLocation
              <> " paremeters."
    Just otherType ->
      paramCodeGenError paramName operationKey $
        "Unsupported schema type found for param: "
          <> show otherType
    Nothing ->
      paramCodeGenError paramName operationKey $
        "No schema type found."

mkSchemaMap :: T.Text -> OA.Schema -> CGU.CodeGen SchemaMap
mkSchemaMap schemaKey schema = do
  (_moduleName, typeName) <- CGU.inferTypeForInputName CGU.Type schemaKey
  baseSchemaInfo <- CGU.inferSchemaInfoForInputName schemaKey

  (inlinedTypes, dataFormat) <- mkOpenApiDataFormat schemaKey schema

  let
    schemaInfo =
      case OA._schemaNullable schema of
        Just True -> CGU.nullableTypeInfo baseSchemaInfo
        _ -> baseSchemaInfo

    codeGenType =
      CGU.CodeGenType
        { CGU.codeGenTypeOriginalName = schemaKey
        , CGU.codeGenTypeName = typeName
        , CGU.codeGenTypeSchemaInfo = schemaInfo
        , CGU.codeGenTypeDescription = OA._schemaDescription schema
        , CGU.codeGenTypeDataFormat = dataFormat
        }

    schemaEntry =
      SchemaEntry
        { schemaOpenApiSchema = schema
        , schemaCodeGenType = codeGenType
        }

  pure $ Map.singleton schemaKey schemaEntry <> inlinedTypes

mkOpenApiDataFormat :: T.Text -> OA.Schema -> CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiDataFormat typeName schema =
  let
    noRefs mkFormat = do
      dataFormat <- mkFormat
      pure (Map.empty, dataFormat)
  in
    case OA._schemaType schema of
      Just OA.OpenApiString -> noRefs $ mkOpenApiStringFormat schema
      Just OA.OpenApiNumber -> noRefs $ mkOpenApiNumberFormat schema
      Just OA.OpenApiInteger -> noRefs $ mkOpenApiIntegerFormat schema
      Just OA.OpenApiBoolean -> noRefs $ pure CGU.boolFormat
      Just OA.OpenApiArray -> mkOpenApiArrayFormat typeName schema
      Just OA.OpenApiObject -> mkOpenApiObjectFormat typeName schema
      Just OA.OpenApiNull -> noRefs $ pure CGU.nullFormat
      Nothing -> mkOpenApiObjectFormat typeName schema

mkOpenApiStringFormat :: OA.Schema -> CGU.CodeGen CGU.CodeGenDataFormat
mkOpenApiStringFormat schema =
  case OA._schemaEnum schema of
    Just enumValues ->
      fmap
        (CGU.enumFormat . catMaybes)
        (traverse (enumValueToText schema) enumValues)
    Nothing ->
      pure $
        case OA._schemaFormat schema of
          Just "date" -> CGU.dayFormat
          Just "date-time" -> CGU.utcTimeFormat
          _ -> CGU.textFormat

enumValueToText :: OA.Schema -> Aeson.Value -> CGU.CodeGen (Maybe T.Text)
enumValueToText schema value =
  case value of
    Aeson.String text -> pure (Just text)
    Aeson.Null ->
      case OA._schemaNullable schema of
        Just True -> pure Nothing
        _ -> CGU.codeGenError "null listed as enum value in a non-nullable schema"
    _ -> CGU.codeGenError "Non-string value found for enum"

mkOpenApiNumberFormat :: OA.Schema -> CGU.CodeGen CGU.CodeGenDataFormat
mkOpenApiNumberFormat schema =
  pure $
    case OA._schemaFormat schema of
      Just "float" -> CGU.floatFormat
      Just "double" -> CGU.doubleFormat
      _ -> CGU.scientificFormat

mkOpenApiIntegerFormat :: OA.Schema -> CGU.CodeGen CGU.CodeGenDataFormat
mkOpenApiIntegerFormat schema =
  pure $
    case OA._schemaFormat schema of
      Just "int32" -> CGU.int32Format
      Just "int64" -> CGU.int64Format
      _ -> CGU.integerFormat

mkOpenApiObjectFormat ::
  T.Text ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiObjectFormat schemaKey schema = do
  let
    requiredParams =
      OA._schemaRequired schema

  (schemaMaps, fields) <-
    fmap unzip
      . traverse (uncurry $ propertyToCodeGenField schemaKey requiredParams)
      . filter (\(prop, _) -> not (prop `elem` unsupportedProperties))
      . IOHM.toList
      . OA._schemaProperties
      $ schema

  pure (Map.unions schemaMaps, CGU.CodeGenObject fields)

unsupportedProperties :: [T.Text]
unsupportedProperties =
  [ "_links"
  ]

mkOpenApiArrayFormat ::
  T.Text ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiArrayFormat schemaKey schema = do
  fmap (fmap CGU.CodeGenArray) $
    schemaArrayItemsToFieldType
      schemaKey
      schema
      schemaKey
      (OA._schemaItems schema)

propertyToCodeGenField ::
  T.Text ->
  [OA.ParamName] ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenObjectField)
propertyToCodeGenField parentSchemaKey requiredParams name schemaRef = do
  (schemaMap, codeGenFieldType) <-
    schemaRefToFieldType parentSchemaKey name schemaRef

  let
    field =
      CGU.CodeGenObjectField
        { CGU.codeGenFieldName = name
        , CGU.codeGenFieldRequired = name `elem` requiredParams
        , CGU.codeGenFieldType = codeGenFieldType
        }

  pure (schemaMap, field)

schemaRefToFieldType ::
  T.Text ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenObjectFieldType)
schemaRefToFieldType parentKey fieldName schemaRef =
  case schemaRef of
    OA.Ref ref ->
      pure (Map.empty, CGU.TypeReference . OA.getReference $ ref)
    OA.Inline inline ->
      case OA._schemaType inline of
        Just OA.OpenApiArray ->
          let
            nullable =
              OA._schemaNullable inline == Just True
          in
            fmap (fmap (CGU.CodeGenFieldArray nullable)) $
              schemaArrayItemsToFieldType
                parentKey
                inline
                fieldName
                (OA._schemaItems inline)
        _ -> do
          let
            key =
              parentKey <> "." <> fieldName

            childRef =
              CGU.TypeReference key

          schemaMap <- mkSchemaMap key inline
          pure (schemaMap, childRef)

schemaArrayItemsToFieldType ::
  T.Text ->
  OA.Schema ->
  OA.ParamName ->
  Maybe OA.OpenApiItems ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenObjectFieldType)
schemaArrayItemsToFieldType parentKey schema fieldName arrayItems =
  let
    fieldError err =
      CGU.codeGenError $
        "Unable to generate type for field "
          <> show fieldName
          <> " of object "
          <> show parentKey
          <> ": "
          <> err
  in
    case arrayItems of
      Just (OA.OpenApiItemsObject itemSchema) ->
        schemaRefToFieldType parentKey (fieldName <> "Item") itemSchema
      Just (OA.OpenApiItemsArray []) -> do
        let
          key =
            fieldName <> "Item"

          fieldType =
            CGU.TypeReference key

        (_moduleName, typeName) <- CGU.inferTypeForInputName CGU.Type key
        schemaTypeInfo <- CGU.inferSchemaInfoForInputName key

        let
          schemaMap =
            Map.singleton key $
              SchemaEntry
                { schemaOpenApiSchema = schema
                , schemaCodeGenType =
                    CGU.CodeGenType
                      { CGU.codeGenTypeOriginalName = key
                      , CGU.codeGenTypeName = typeName
                      , CGU.codeGenTypeSchemaInfo = schemaTypeInfo
                      , CGU.codeGenTypeDescription = Nothing
                      , CGU.codeGenTypeDataFormat = CGU.textFormat
                      }
                }

        pure (schemaMap, fieldType)
      Just (OA.OpenApiItemsArray _itemSchemaRefs) ->
        fieldError "Heterogeneous arrays are not supported"
      Nothing ->
        fieldError "Array schema found with no item schema"
