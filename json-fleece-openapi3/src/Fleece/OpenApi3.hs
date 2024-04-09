{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Fleece.OpenApi3
  ( generateOpenApiFleeceCode
  ) where

import Control.Monad (join, (<=<))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap, first)
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.NonEmptyText as NET
import qualified Data.OpenApi as OA
import qualified Data.Text as T

import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.HaskellCode as HC

generateOpenApiFleeceCode ::
  OA.OpenApi ->
  CGU.CodeGen CGU.Modules
generateOpenApiFleeceCode openApi = do
  typeMap <- mkCodeGenTypes openApi
  CGU.generateFleeceCode typeMap

type SchemaMap =
  Map.Map CGU.CodeGenKey SchemaEntry

data SchemaEntry = SchemaEntry
  { schemaCodeGenType :: CGU.CodeGenType
  , schemaOpenApiSchema :: OA.Schema
  }

unionsErrorOnConflict ::
  [Map.Map CGU.CodeGenKey a] ->
  CGU.CodeGen (Map.Map CGU.CodeGenKey a)
unionsErrorOnConflict maps =
  let
    conflictOnError key _a _b =
      CGU.codeGenError ("Duplicate key found: " <> show key)
  in
    sequence $
      foldr
        (Map.unionWithKey conflictOnError)
        mempty
        (fmap (fmap pure) maps)

mkCodeGenTypes :: OA.OpenApi -> CGU.CodeGen CGU.CodeGenMap
mkCodeGenTypes openApi = do
  schemaMaps <-
    traverse (uncurry (mkSchemaMap CGU.Type))
      . IOHM.toList
      . OA._componentsSchemas
      . OA._openApiComponents
      $ openApi

  schemaMap <- unionsErrorOnConflict schemaMaps

  let
    pathItems =
      IOHM.toList
        . OA._openApiPaths
        $ openApi

    codeGenMap =
      fmap (CGU.CodeGenItemType . schemaCodeGenType) schemaMap

  pathTypes <- traverse (uncurry $ mkPathItem schemaMap) pathItems
  unionsErrorOnConflict (codeGenMap : pathTypes)

mkPathItem :: SchemaMap -> FilePath -> OA.PathItem -> CGU.CodeGen CGU.CodeGenMap
mkPathItem schemaMap filePath pathItem = do
  let
    methodOperations =
      pathItemOperations pathItem

    nameStrategy =
      if length methodOperations > 1
        then FallbackOperationNameIncludeMethod
        else FallbackOperationNameOmitMethod

  operationCodeGenMaps <-
    traverse
      (uncurry $ mkOperation schemaMap filePath pathItem nameStrategy)
      methodOperations

  unionsErrorOnConflict operationCodeGenMaps

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

data FallbackOperationNamingStrategy
  = FallbackOperationNameIncludeMethod
  | FallbackOperationNameOmitMethod

mkOperation ::
  SchemaMap ->
  FilePath ->
  OA.PathItem ->
  FallbackOperationNamingStrategy ->
  T.Text ->
  OA.Operation ->
  CGU.CodeGen CGU.CodeGenMap
mkOperation schemaMap filePath pathItem nameStrategy method operation = do
  let
    pathTextParts =
      filter (not . T.null)
        . T.splitOn "/"
        . T.pack
        $ filePath

    operationKey =
      case OA._operationOperationId operation of
        Just operationId -> operationId
        Nothing ->
          let
            pathKey =
              T.intercalate "." pathTextParts
          in
            case nameStrategy of
              FallbackOperationNameOmitMethod -> pathKey
              FallbackOperationNameIncludeMethod -> pathKey <> "." <> method

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
        , CGU.codeGenOperationRequestBody = fmap schemaTypeInfoDependent mbRequestBodySchema
        , CGU.codeGenOperationResponses = fmap (fmap schemaTypeInfoDependent) responses
        }

    mkParamEntry (paramName, param) =
      ( CGU.ParamKey (operationKey <> "." <> paramName)
      , CGU.CodeGenItemOperationParam param
      )

    paramModules =
      Map.fromList
        . map mkParamEntry
        . Map.toList
        $ params

    requestBodyModules =
      fmap (CGU.CodeGenItemType . schemaCodeGenType)
        . maybe mempty schemaTypeInfoDependencies
        $ mbRequestBodySchema

    responseBodyModules =
      fmap (CGU.CodeGenItemType . schemaCodeGenType)
        . foldMap (maybe mempty schemaTypeInfoDependencies)
        $ responses

  pure $
    Map.singleton (CGU.OperationKey operationKey) (CGU.CodeGenItemOperation codeGenOperation)
      <> paramModules
      <> requestBodyModules
      <> responseBodyModules

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

data SchemaTypeInfoWithDeps = SchemaTypeInfoWithDeps
  { schemaTypeInfoDependent :: CGU.SchemaTypeInfoOrRef
  , schemaTypeInfoDependencies :: SchemaMap
  }

schemaInfoWithoutDependencies :: CGU.SchemaTypeInfo -> SchemaTypeInfoWithDeps
schemaInfoWithoutDependencies schemaTypeInfo =
  SchemaTypeInfoWithDeps
    { schemaTypeInfoDependent = Left schemaTypeInfo
    , schemaTypeInfoDependencies = Map.empty
    }

fmapSchemaInfoAndDeps ::
  (CGU.SchemaTypeInfoOrRef -> CGU.SchemaTypeInfoOrRef) ->
  SchemaTypeInfoWithDeps ->
  SchemaTypeInfoWithDeps
fmapSchemaInfoAndDeps f schemaTypeInfoWithDeps =
  schemaTypeInfoWithDeps
    { schemaTypeInfoDependent = f $ schemaTypeInfoDependent schemaTypeInfoWithDeps
    }

lookupRequestBodySchema ::
  T.Text ->
  SchemaMap ->
  OA.MediaTypeObject ->
  CGU.CodeGen (Maybe SchemaTypeInfoWithDeps)
lookupRequestBodySchema operationKey schemaMap mediaTypeObject =
  let
    requestError msg =
      CGU.codeGenError $
        "Error finding request body schema for operation "
          <> show operationKey
          <> ": "
          <> msg
  in
    case OA._mediaTypeObjectSchema mediaTypeObject of
      Just (OA.Ref (OA.Reference refKey)) ->
        case Map.lookup (CGU.SchemaKey refKey) schemaMap of
          Just schemaEntry ->
            pure
              . Just
              . schemaInfoWithoutDependencies
              . CGU.codeGenTypeSchemaInfo
              . schemaCodeGenType
              $ schemaEntry
          Nothing ->
            requestError $
              "Unable to resolve schema reference "
                <> show refKey
                <> "."
      Just (OA.Inline schema) -> do
        fmap Just $
          mkInlineBodySchema
            requestError
            (operationKey <> ".RequestBody")
            schemaMap
            schema
      Nothing ->
        pure Nothing

lookupResponses ::
  T.Text ->
  SchemaMap ->
  OA.Responses ->
  CGU.CodeGen (Map.Map CGU.ResponseStatus (Maybe SchemaTypeInfoWithDeps))
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
    Map.traverseWithKey
      (lookupResponseBodySchema operationKey schemaMap)
      allEntries

lookupResponseBodySchema ::
  T.Text ->
  SchemaMap ->
  CGU.ResponseStatus ->
  OA.Referenced OA.Response ->
  CGU.CodeGen (Maybe SchemaTypeInfoWithDeps)
lookupResponseBodySchema operationKey schemaMap responseStatus responseRef =
  let
    responseError msg =
      CGU.codeGenError $
        "Error looking up response for operation "
          <> show operationKey
          <> ": "
          <> msg

    lookupCodeGenType refKey =
      case Map.lookup (CGU.SchemaKey refKey) schemaMap of
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
            fmap Just $
              case OA._mediaTypeObjectSchema mediaTypeObject of
                Just (OA.Ref (OA.Reference refKey)) ->
                  fmap schemaInfoWithoutDependencies (lookupCodeGenType refKey)
                Just (OA.Inline schema) ->
                  let
                    responseName =
                      T.pack $
                        case responseStatus of
                          CGU.ResponseStatusCode n ->
                            "Response" <> show n <> "Body"
                          CGU.DefaultResponse ->
                            "DefaultResponseBody"
                  in
                    mkInlineBodySchema
                      responseError
                      (operationKey <> "." <> responseName)
                      schemaMap
                      schema
                Nothing ->
                  -- This indicates that the empty schema was specified for
                  -- the media type.
                  pure (schemaInfoWithoutDependencies CGU.anyJSONSchemaTypeInfo)

mkInlineStringSchema ::
  T.Text ->
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineStringSchema schemaKey schema = do
  case OA._schemaEnum schema of
    Nothing -> pure . schemaInfoWithoutDependencies $ CGU.textSchemaTypeInfo
    Just _values -> do
      (_moduleName, typeName) <- CGU.inferTypeForInputName CGU.Operation schemaKey
      (inlinedTypes, schemaTypeInfo) <-
        mkSchemaTypeInfo
          schemaKey
          typeName
          schema

      pure $
        SchemaTypeInfoWithDeps
          { schemaTypeInfoDependent = Left schemaTypeInfo
          , schemaTypeInfoDependencies = inlinedTypes
          }

mkInlineBoolSchema :: CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineBoolSchema =
  pure . schemaInfoWithoutDependencies $ CGU.boolSchemaTypeInfo

mkInlineIntegerSchema ::
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineIntegerSchema schema =
  pure . schemaInfoWithoutDependencies $
    case OA._schemaFormat schema of
      Just "int32" -> CGU.int32SchemaTypeInfo
      Just "int64" -> CGU.int64SchemaTypeInfo
      Just _ -> CGU.integerSchemaTypeInfo
      Nothing -> CGU.integerSchemaTypeInfo

mkInlineBodyObjectSchema ::
  (forall a. String -> CGU.CodeGen a) ->
  T.Text ->
  SchemaMap ->
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineBodyObjectSchema raiseError schemaKey schemaMap schema =
  if IOHM.null (OA._schemaProperties schema)
    then
      mkAdditionalPropertiesMapSchema
        raiseError
        schemaKey
        (\key itemSchema -> mkInlineBodySchema raiseError key schemaMap itemSchema)
        (OA._schemaAdditionalProperties schema)
    else do
      (_moduleName, typeName) <- CGU.inferTypeForInputName CGU.Operation schemaKey
      (fieldsSchemaMap, dataFormat) <-
        mkOpenApiObjectFormat
          CGU.Operation
          schemaKey
          typeName
          schema

      schemaTypeInfo <- CGU.inferSchemaInfoForTypeName typeName

      let
        codeGenType =
          CGU.CodeGenType
            { CGU.codeGenTypeOriginalName = schemaKey
            , CGU.codeGenTypeName = typeName
            , CGU.codeGenTypeSchemaInfo = schemaTypeInfo
            , CGU.codeGenTypeDescription = NET.fromText =<< OA._schemaDescription schema
            , CGU.codeGenTypeDataFormat = dataFormat
            }

        schemaEntry =
          SchemaEntry
            { schemaOpenApiSchema = schema
            , schemaCodeGenType = codeGenType
            }

        codeGenModules =
          Map.insert
            (CGU.SchemaKey schemaKey)
            schemaEntry
            fieldsSchemaMap

      pure $
        SchemaTypeInfoWithDeps
          { schemaTypeInfoDependent = Left schemaTypeInfo
          , schemaTypeInfoDependencies = codeGenModules
          }

mkInlineArraySchema ::
  (forall a. String -> CGU.CodeGen a) ->
  T.Text ->
  SchemaMap ->
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineArraySchema raiseError schemaKey schemaMap schema =
  let
    lookupCodeGenType refKey =
      case Map.lookup (CGU.SchemaKey refKey) schemaMap of
        Just schemaEntry ->
          pure . CGU.codeGenTypeSchemaInfo . schemaCodeGenType $ schemaEntry
        Nothing ->
          raiseError $
            "Unable to resolve schema reference "
              <> show refKey
              <> "."
  in
    case OA._schemaItems schema of
      Just (OA.OpenApiItemsObject (OA.Ref (OA.Reference itemRefKey))) -> do
        itemSchemaInfo <- lookupCodeGenType itemRefKey
        pure . schemaInfoWithoutDependencies . CGU.arrayTypeInfo $ itemSchemaInfo
      Just (OA.OpenApiItemsObject (OA.Inline innerSchema)) ->
        let
          itemKey =
            schemaKey <> "Item"
        in
          fmap
            (fmapSchemaInfoAndDeps $ first CGU.arrayTypeInfo)
            (mkInlineBodySchema raiseError itemKey schemaMap innerSchema)
      otherItemType ->
        raiseError $
          "Unsupported schema array item type found: "
            <> show otherItemType

mkInlineArrayOneOfSchema ::
  (forall a. String -> CGU.CodeGen a) ->
  T.Text ->
  SchemaMap ->
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineArrayOneOfSchema raiseError schemaKey schemaMap schema =
  case OA._schemaItems schema of
    Just (OA.OpenApiItemsObject (OA.Ref ref)) -> do
      pure $
        SchemaTypeInfoWithDeps
          { schemaTypeInfoDependent = Right $ CGU.CodeGenRefArray $ CGU.TypeReference $ OA.getReference ref
          , schemaTypeInfoDependencies = mempty
          }
    Just (OA.OpenApiItemsObject (OA.Inline innerSchema)) ->
      let
        itemKey =
          schemaKey <> "Item"
      in
        fmap
          (fmapSchemaInfoAndDeps (bimap CGU.arrayTypeInfo CGU.CodeGenRefArray))
          (mkInlineOneOfSchema raiseError itemKey schemaMap innerSchema)
    otherItemType ->
      raiseError $
        "Unsupported schema array item type found: "
          <> show otherItemType

applyNullable :: OA.Schema -> SchemaTypeInfoWithDeps -> SchemaTypeInfoWithDeps
applyNullable schema =
  if OA._schemaNullable schema == Just True
    then fmapSchemaInfoAndDeps (bimap CGU.nullableTypeInfo CGU.CodeGenRefNullable)
    else id

mkInlineBodySchema ::
  (forall a. String -> CGU.CodeGen a) ->
  T.Text ->
  SchemaMap ->
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineBodySchema raiseError schemaKey schemaMap schema =
  applyNullable schema <$> case OA._schemaType schema of
    Just OA.OpenApiArray -> mkInlineArraySchema raiseError schemaKey schemaMap schema
    Just OA.OpenApiString -> mkInlineStringSchema schemaKey schema
    Just OA.OpenApiBoolean -> mkInlineBoolSchema
    Just OA.OpenApiInteger -> mkInlineIntegerSchema schema
    Just OA.OpenApiObject -> mkInlineBodyObjectSchema raiseError schemaKey schemaMap schema
    Just s -> raiseError $ "Inline " <> show s <> " schemas are not currently supported."
    Nothing -> raiseError "Inline schema doesn't have a type."

mkInlineOneOfSchema ::
  (forall a. String -> CGU.CodeGen a) ->
  T.Text ->
  SchemaMap ->
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkInlineOneOfSchema raiseError schemaKey schemaMap schema =
  applyNullable schema <$> case OA._schemaType schema of
    Just OA.OpenApiArray -> mkInlineArrayOneOfSchema raiseError schemaKey schemaMap schema
    Just OA.OpenApiString -> mkInlineStringSchema schemaKey schema
    Just OA.OpenApiBoolean -> mkInlineBoolSchema
    Just OA.OpenApiInteger -> mkInlineIntegerSchema schema
    Just OA.OpenApiObject -> raiseError "Inline OpenApiObject schemas are not currently supported in oneOf."
    Just s -> raiseError $ "Inline " <> show s <> " schemas are not currently supported."
    Nothing -> raiseError "Inline schema doesn't have a type."

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

      typeOptions <- CGU.lookupTypeOptions paramTypeName

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
          , CGU.codeGenOperationParamTypeOptions = typeOptions
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
      case Map.lookup (CGU.SchemaKey refKey) schemaMap of
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
            traverse (rejectNull <=< enumValueToText paramName schema) enumValues

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
      let
        arrayParamSchema =
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
      in
        case paramLocation of
          OA.ParamQuery -> arrayParamSchema
          OA.ParamHeader -> arrayParamSchema
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

mkSchemaMap :: CGU.CodeSection -> T.Text -> OA.Schema -> CGU.CodeGen SchemaMap
mkSchemaMap section schemaKey schema = do
  (_moduleName, typeName) <- CGU.inferTypeForInputName section schemaKey
  fmap fst (mkSchemaTypeInfo schemaKey typeName schema)

mkSchemaTypeInfo ::
  T.Text ->
  HC.TypeName ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.SchemaTypeInfo)
mkSchemaTypeInfo schemaKey typeName schema = do
  baseSchemaInfo <- CGU.inferSchemaInfoForTypeName typeName
  (inlinedTypes, dataFormat) <- mkOpenApiDataFormat schemaKey typeName schema

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
        , CGU.codeGenTypeDescription = NET.fromText =<< OA._schemaDescription schema
        , CGU.codeGenTypeDataFormat = dataFormat
        }

    schemaEntry =
      SchemaEntry
        { schemaOpenApiSchema = schema
        , schemaCodeGenType = codeGenType
        }

    schemaMap =
      Map.singleton (CGU.SchemaKey schemaKey) schemaEntry <> inlinedTypes

  pure (schemaMap, schemaInfo)

mkOpenApiDataFormat ::
  T.Text ->
  HC.TypeName ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiDataFormat schemaKey typeName schema =
  let
    noRefs mkFormat = do
      dataFormat <- mkFormat
      pure (Map.empty, dataFormat)
  in
    case OA._schemaType schema of
      Just OA.OpenApiString -> noRefs $ mkOpenApiStringFormat typeName schema
      Just OA.OpenApiNumber -> noRefs $ mkOpenApiNumberFormat typeName schema
      Just OA.OpenApiInteger -> noRefs $ mkOpenApiIntegerFormat typeName schema
      Just OA.OpenApiBoolean -> do
        typeOptions <- CGU.lookupTypeOptions typeName
        noRefs $ pure (CGU.boolFormat typeOptions)
      Just OA.OpenApiArray -> mkOpenApiArrayFormat schemaKey typeName schema
      Just OA.OpenApiObject -> mkOpenApiObjectFormatOrAdditionalPropertiesNewtype CGU.Type schemaKey typeName schema
      Just OA.OpenApiNull -> do
        typeOptions <- CGU.lookupTypeOptions typeName
        noRefs $ pure (CGU.nullFormat typeOptions)
      Nothing ->
        case OA._schemaOneOf schema of
          Just schemas ->
            mkOneOf schemaKey schemas
          Nothing ->
            mkOpenApiObjectFormatOrAdditionalPropertiesNewtype CGU.Type schemaKey typeName schema

mkOneOf ::
  T.Text ->
  [OA.Referenced OA.Schema] ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOneOf schemaKey refSchemas =
  let
    processRefSchema refSchema =
      case refSchema of
        OA.Inline schema -> do
          typeInfoWithDeps <-
            mkInlineOneOfSchema
              (\err -> CGU.codeGenError $ "Inside inline oneOf: " <> err)
              schemaKey
              mempty
              schema
          let
            unionMember =
              CGU.CodeGenUnionMember
                { CGU.codeGenUnionMemberType = schemaTypeInfoDependent typeInfoWithDeps
                }
          pure (schemaTypeInfoDependencies typeInfoWithDeps, unionMember)
        OA.Ref ref -> do
          let
            unionMember =
              CGU.CodeGenUnionMember
                { CGU.codeGenUnionMemberType = Right $ CGU.TypeReference $ OA.getReference ref
                }
          pure (mempty, unionMember)
  in
    do
      (maps, codeGenUnionMembers) <- fmap unzip . traverse processRefSchema $ refSchemas
      schemaMap <- unionsErrorOnConflict maps
      pure (schemaMap, CGU.CodeGenUnion codeGenUnionMembers)

mkOpenApiStringFormat :: HC.TypeName -> OA.Schema -> CGU.CodeGen CGU.CodeGenDataFormat
mkOpenApiStringFormat typeName schema = do
  typeOptions <- CGU.lookupTypeOptions typeName
  case OA._schemaEnum schema of
    Just enumValues ->
      fmap
        (CGU.enumFormat typeOptions . catMaybes)
        (traverse (enumValueToText (HC.typeNameText typeName) schema) enumValues)
    Nothing ->
      pure $
        case OA._schemaFormat schema of
          Just "date" ->
            case CGU.dateFormat typeOptions of
              CGU.ISO8601DateFormat -> CGU.dayFormat typeOptions
              CGU.CustomDateFormat formatString -> CGU.dayCustomFormat formatString typeOptions
          Just "date-time" ->
            case CGU.dateTimeFormat typeOptions of
              CGU.UTCTimeFormat -> CGU.utcTimeFormat typeOptions
              CGU.ZonedTimeFormat -> CGU.zonedTimeFormat typeOptions
              CGU.LocalTimeFormat -> CGU.localTimeFormat typeOptions
          _ -> CGU.textFormat typeOptions

enumValueToText :: T.Text -> OA.Schema -> Aeson.Value -> CGU.CodeGen (Maybe T.Text)
enumValueToText name schema value =
  case value of
    Aeson.String text -> pure (Just text)
    Aeson.Null ->
      case OA._schemaNullable schema of
        Just True -> pure Nothing
        _ -> CGU.codeGenError "null listed as enum value in a non-nullable schema"
    _ ->
      CGU.codeGenError $
        "Non-string value found for enum in schema/parameter titled '"
          <> T.unpack name
          <> "', value is "
          <> show value

mkOpenApiNumberFormat :: HC.TypeName -> OA.Schema -> CGU.CodeGen CGU.CodeGenDataFormat
mkOpenApiNumberFormat typeName schema = do
  typeOptions <- CGU.lookupTypeOptions typeName
  pure $
    case OA._schemaFormat schema of
      Just "float" -> CGU.floatFormat typeOptions
      Just "double" -> CGU.doubleFormat typeOptions
      _ -> CGU.scientificFormat typeOptions

mkOpenApiIntegerFormat :: HC.TypeName -> OA.Schema -> CGU.CodeGen CGU.CodeGenDataFormat
mkOpenApiIntegerFormat typeName schema = do
  typeOptions <- CGU.lookupTypeOptions typeName
  pure $
    case OA._schemaFormat schema of
      Just "int32" -> CGU.int32Format typeOptions
      Just "int64" -> CGU.int64Format typeOptions
      _ -> CGU.integerFormat typeOptions

mkOpenApiObjectFormatOrAdditionalPropertiesNewtype ::
  CGU.CodeSection ->
  T.Text ->
  HC.TypeName ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiObjectFormatOrAdditionalPropertiesNewtype section schemaKey typeName schema = do
  if IOHM.null (OA._schemaProperties schema)
    then
      mkOpenApiAdditionalPropertiesNewtype
        section
        schemaKey
        typeName
        schema
    else
      mkOpenApiObjectFormat
        section
        schemaKey
        typeName
        schema

mkOpenApiAdditionalPropertiesNewtype ::
  CGU.CodeSection ->
  T.Text ->
  HC.TypeName ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiAdditionalPropertiesNewtype section schemaKey typeName schema = do
  let
    raiseError err =
      CGU.codeGenError $
        "Unable to build schema for "
          <> show schemaKey
          <> ": "
          <> err

  schemaTypeInfoWithDeps <-
    mkAdditionalPropertiesMapSchema
      raiseError
      schemaKey
      (mkAdditionalPropertiesInlineItemSchema section)
      (OA._schemaAdditionalProperties schema)

  typeOptions <- CGU.lookupTypeOptions typeName

  let
    format =
      CGU.CodeGenNewType
        typeOptions
        (schemaTypeInfoDependent schemaTypeInfoWithDeps)
  pure (schemaTypeInfoDependencies schemaTypeInfoWithDeps, format)

mkOpenApiObjectFormat ::
  CGU.CodeSection ->
  T.Text ->
  HC.TypeName ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiObjectFormat section schemaKey typeName schema = do
  let
    requiredParams =
      OA._schemaRequired schema

    raiseAdditionalPropsError err =
      CGU.codeGenError $
        "Unable to build additionalProperties schema for "
          <> show schemaKey
          <> ": "
          <> err

  typeOptions <- CGU.lookupTypeOptions typeName

  (fieldDependencies, fields) <-
    fmap unzip
      . traverse (uncurry $ propertyToCodeGenField section schemaKey requiredParams)
      . filter (\(prop, _) -> prop `notElem` unsupportedProperties)
      . IOHM.toList
      . OA._schemaProperties
      $ schema

  mbAdditionalProperties <-
    case OA._schemaAdditionalProperties schema of
      Nothing ->
        pure Nothing
      Just additionalProperties ->
        fmap Just $
          mkAdditionalPropertiesSchema
            raiseAdditionalPropsError
            schemaKey
            (mkAdditionalPropertiesInlineItemSchema section)
            (Just additionalProperties)

  let
    dependencies =
      Map.unions
        ( maybe Map.empty schemaTypeInfoDependencies mbAdditionalProperties
            : fieldDependencies
        )

    mbCodeGenAdditionalProps =
      fmap
        (CGU.CodeGenAdditionalProperties . schemaTypeInfoDependent)
        mbAdditionalProperties

  pure (dependencies, CGU.CodeGenObject typeOptions fields mbCodeGenAdditionalProps)

mkAdditionalPropertiesInlineItemSchema ::
  CGU.CodeSection ->
  T.Text ->
  OA.Schema ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkAdditionalPropertiesInlineItemSchema section itemKey itemSchema = do
  itemDependencies <- mkSchemaMap section itemKey itemSchema
  (_moduleName, itemTypeName) <- CGU.inferTypeForInputName section itemKey
  itemSchemaInfo <- CGU.inferSchemaInfoForTypeName itemTypeName
  pure $
    SchemaTypeInfoWithDeps
      { schemaTypeInfoDependent = Left itemSchemaInfo
      , schemaTypeInfoDependencies = itemDependencies
      }

unsupportedProperties :: [T.Text]
unsupportedProperties =
  [ "_links"
  ]

mkOpenApiArrayFormat ::
  T.Text ->
  HC.TypeName ->
  OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenDataFormat)
mkOpenApiArrayFormat schemaKey typeName schema = do
  typeOptions <- CGU.lookupTypeOptions typeName
  fmap (fmap (CGU.CodeGenArray typeOptions)) $
    schemaArrayItemsToFieldType
      CGU.Type
      schemaKey
      schema
      schemaKey
      (OA._schemaItems schema)

propertyToCodeGenField ::
  CGU.CodeSection ->
  T.Text ->
  [OA.ParamName] ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenObjectField)
propertyToCodeGenField section parentSchemaKey requiredParams name schemaRef = do
  (schemaMap, codeGenFieldType) <-
    schemaRefToFieldType section parentSchemaKey name schemaRef

  let
    field =
      CGU.CodeGenObjectField
        { CGU.codeGenFieldName = name
        , CGU.codeGenFieldRequired = name `elem` requiredParams
        , CGU.codeGenFieldType = codeGenFieldType
        }

  pure (schemaMap, field)

schemaRefToFieldType ::
  CGU.CodeSection ->
  T.Text ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenRefType)
schemaRefToFieldType section parentKey fieldName schemaRef =
  case schemaRef of
    OA.Ref ref ->
      pure (Map.empty, CGU.TypeReference . OA.getReference $ ref)
    OA.Inline inlineSchema ->
      case OA._schemaType inlineSchema of
        Just OA.OpenApiArray ->
          let
            nullable =
              OA._schemaNullable inlineSchema == Just True
            applyNull =
              if nullable
                then CGU.CodeGenRefNullable
                else id
          in
            fmap (fmap (applyNull . CGU.CodeGenRefArray)) $
              schemaArrayItemsToFieldType
                section
                parentKey
                inlineSchema
                fieldName
                (OA._schemaItems inlineSchema)
        _ -> do
          let
            key =
              parentKey <> "." <> fieldName

            childRef =
              CGU.TypeReference key

          schemaMap <- mkSchemaMap section key inlineSchema
          pure (schemaMap, childRef)

schemaArrayItemsToFieldType ::
  CGU.CodeSection ->
  T.Text ->
  OA.Schema ->
  OA.ParamName ->
  Maybe OA.OpenApiItems ->
  CGU.CodeGen (SchemaMap, CGU.CodeGenRefType)
schemaArrayItemsToFieldType section parentKey schema fieldName arrayItems =
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
        schemaRefToFieldType section parentKey (fieldName <> "Item") itemSchema
      Just (OA.OpenApiItemsArray []) -> do
        let
          key =
            fieldName <> "Item"

          fieldType =
            CGU.TypeReference key

        (_moduleName, typeName) <- CGU.inferTypeForInputName section key
        schemaTypeInfo <- CGU.inferSchemaInfoForTypeName typeName
        typeOptions <- CGU.lookupTypeOptions typeName

        let
          schemaMap =
            Map.singleton (CGU.SchemaKey key) $
              SchemaEntry
                { schemaOpenApiSchema = schema
                , schemaCodeGenType =
                    CGU.CodeGenType
                      { CGU.codeGenTypeOriginalName = key
                      , CGU.codeGenTypeName = typeName
                      , CGU.codeGenTypeSchemaInfo = schemaTypeInfo
                      , CGU.codeGenTypeDescription = Nothing
                      , CGU.codeGenTypeDataFormat = CGU.textFormat typeOptions
                      }
                }

        pure (schemaMap, fieldType)
      Just (OA.OpenApiItemsArray _itemSchemaRefs) ->
        fieldError "Heterogeneous arrays are not supported"
      Nothing ->
        fieldError "Array schema found with no item schema"

mkAdditionalPropertiesMapSchema ::
  (forall a. String -> CGU.CodeGen a) ->
  T.Text ->
  (T.Text -> OA.Schema -> CGU.CodeGen SchemaTypeInfoWithDeps) ->
  Maybe OA.AdditionalProperties ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkAdditionalPropertiesMapSchema raiseError schemaKey mkInlineItemSchema mbAdditionalProperties =
  fmap (fmapSchemaInfoAndDeps $ bimap CGU.mapTypeInfo CGU.CodeGenRefMap) $
    mkAdditionalPropertiesSchema
      raiseError
      schemaKey
      mkInlineItemSchema
      mbAdditionalProperties

mkAdditionalPropertiesSchema ::
  (forall a. String -> CGU.CodeGen a) ->
  T.Text ->
  (T.Text -> OA.Schema -> CGU.CodeGen SchemaTypeInfoWithDeps) ->
  Maybe OA.AdditionalProperties ->
  CGU.CodeGen SchemaTypeInfoWithDeps
mkAdditionalPropertiesSchema raiseError schemaKey mkInlineItemSchema mbAdditionalProperties =
  case mbAdditionalProperties of
    Nothing ->
      -- No explicit properties nor additional properties are defined,
      -- but the OpenAPI spec defines additional properties as
      -- defaulting to True, so we handle this the same as if only
      -- additional properties was defined as true.
      pure
        . schemaInfoWithoutDependencies
        $ CGU.anyJSONSchemaTypeInfo
    Just (OA.AdditionalPropertiesAllowed True) ->
      pure
        . schemaInfoWithoutDependencies
        $ CGU.anyJSONSchemaTypeInfo
    Just (OA.AdditionalPropertiesAllowed False) ->
      raiseError "Schemas for objects with additional properties disallowed are not yet supported."
    Just (OA.AdditionalPropertiesSchema (OA.Ref ref)) ->
      pure $
        SchemaTypeInfoWithDeps
          { schemaTypeInfoDependent = Right $ CGU.TypeReference $ OA.getReference ref
          , schemaTypeInfoDependencies = Map.empty
          }
    Just (OA.AdditionalPropertiesSchema (OA.Inline innerSchema)) ->
      let
        itemKey =
          schemaKey <> "Item"
      in
        mkInlineItemSchema itemKey innerSchema
