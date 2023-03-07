{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3
  ( generateOpenApiFleeceCode
  , mkSchemaTypeMap
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.OpenApi as OA
import qualified Data.Text as T

import qualified Fleece.CodeGenUtil as CGU

generateOpenApiFleeceCode ::
  OA.OpenApi ->
  CGU.CodeGen CGU.Modules
generateOpenApiFleeceCode swagger = do
  typeMap <- mkCodeGenTypes swagger
  CGU.generateFleeceCode typeMap

mkCodeGenTypes :: OA.OpenApi -> CGU.CodeGen CGU.TypeMap
mkCodeGenTypes openApi = do
  typeMap <-
    fmap Map.unions
      . traverse (uncurry mkSchemaTypeMap)
      . IOHM.toList
      . OA._componentsSchemas
      . OA._openApiComponents
      $ openApi

  let
    pathItems =
      IOHM.toList
        . OA._openApiPaths
        $ openApi

  pathTypes <- traverse (uncurry $ mkPathItem typeMap) pathItems
  pure $ Map.unions (typeMap : pathTypes)

mkPathItem :: CGU.TypeMap -> FilePath -> OA.PathItem -> CGU.CodeGen CGU.TypeMap
mkPathItem typeMap filePath pathItem =
  fmap Map.unions $
    traverse
      (uncurry $ mkOperation typeMap filePath pathItem)
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
  CGU.TypeMap ->
  FilePath ->
  OA.PathItem ->
  T.Text ->
  OA.Operation ->
  CGU.CodeGen CGU.TypeMap
mkOperation typeMap filePath pathItem method operation = do
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
    mkOperationParams typeMap operationKey pathItem operation

  let
    lookupParam name =
      case Map.lookup name params of
        Just param ->
          pure param
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
        then CGU.PathParamRef <$> lookupParam (T.drop 1 . T.dropEnd 1 $ text)
        else pure (CGU.PathLiteral text)

  pathPieces <- traverse mkPiece pathTextParts

  let
    codeGenOperation =
      CGU.CodeGenOperation
        { CGU.codeGenOperationOriginalName = operationKey
        , CGU.codeGenOperationMethod = method
        , CGU.codeGenOperationPath = pathPieces
        }

    mkParamEntry (paramName, param) =
      ( operationKey <> "." <> paramName
      , CGU.CodeGenItemOperationParam param
      )

    paramTypes =
      Map.fromList
        . map mkParamEntry
        . Map.toList
        $ params

  pure $
    Map.singleton operationKey (CGU.CodeGenItemOperation codeGenOperation)
      <> paramTypes

mkOperationParams ::
  CGU.TypeMap ->
  T.Text ->
  OA.PathItem ->
  OA.Operation ->
  CGU.CodeGen (Map.Map T.Text CGU.CodeGenOperationParam)
mkOperationParams typeMap operationKey pathItem operation = do
  paramList <-
    traverse
      (mkOperationParam typeMap operationKey)
      (OA._pathItemParameters pathItem <> OA._operationParameters operation)

  let
    paramMap =
      Map.fromList
        . map (\param -> (CGU.codeGenOperationParamName param, param))
        $ paramList

  pure paramMap

mkOperationParam ::
  CGU.TypeMap ->
  T.Text ->
  OA.Referenced OA.Param ->
  CGU.CodeGen CGU.CodeGenOperationParam
mkOperationParam _typeMap operationKey paramRef = do
  param <-
    case paramRef of
      OA.Ref _ -> CGU.codeGenError "Param refs not yet implemethed"
      OA.Inline param -> pure param

  let
    paramName =
      OA._paramName param

  (_moduleName, paramTypeName) <-
    CGU.inferTypeForInputName CGU.Operation (operationKey <> "." <> paramName)

  schema <-
    case OA._paramSchema param of
      Just (OA.Inline schema) ->
        pure schema
      Just (OA.Ref _) ->
        CGU.codeGenError "Param schema refs not yet implemeted"
      Nothing ->
        CGU.codeGenError $
          "No schema found for param "
            <> T.unpack paramName
            <> " of operation "
            <> T.unpack operationKey

  paramType <-
    schemaTypeToParamType
      paramName
      (OA._paramIn param)
      operationKey
      schema

  pure $
    CGU.CodeGenOperationParam
      { CGU.codeGenOperationParamName = paramName
      , CGU.codeGenOperationParamTypeName = paramTypeName
      , CGU.codeGenOperationParamType = paramType
      }

schemaTypeToParamType ::
  T.Text ->
  OA.ParamLocation ->
  T.Text ->
  OA.Schema ->
  CGU.CodeGen CGU.OperationParamType
schemaTypeToParamType paramName paramLocation operationKey schema =
  case OA._schemaType schema of
    Just OA.OpenApiString ->
      pure CGU.ParamTypeString
    Just OA.OpenApiInteger ->
      case OA._schemaFormat schema of
        Just "int8" -> pure CGU.ParamTypeInt8
        Just "int16" -> pure CGU.ParamTypeInt16
        Just "int32" -> pure CGU.ParamTypeInt32
        Just "int64" -> pure CGU.ParamTypeInt64
        _ -> pure CGU.ParamTypeInteger
    Just OA.OpenApiArray ->
      case paramLocation of
        OA.ParamQuery ->
          case OA._schemaItems schema of
            Just (OA.OpenApiItemsObject itemSchemaRef) ->
              case itemSchemaRef of
                OA.Ref _ ->
                  CGU.codeGenError "Schema refs not yet support in parameter arrays"
                OA.Inline itemSchema ->
                  CGU.ParamTypeArray
                    <$> schemaTypeToParamType
                      paramName
                      paramLocation
                      operationKey
                      itemSchema
            otherItemType ->
              CGU.codeGenError $
                "Unsupported schema arram item type found for param "
                  <> T.unpack paramName
                  <> " of operation "
                  <> T.unpack operationKey
                  <> ": "
                  <> show otherItemType
        otherLocation ->
          CGU.codeGenError $
            "Array parameters are not supported for "
              <> show otherLocation
              <> " paremeters. Parameter in question was "
              <> T.unpack paramName
              <> " of operation "
              <> T.unpack operationKey
    Just otherType ->
      CGU.codeGenError $
        "Unsupported schema type found for param "
          <> T.unpack paramName
          <> " of operation "
          <> T.unpack operationKey
          <> ": "
          <> show otherType
    Nothing ->
      CGU.codeGenError $
        "No schema type found for param "
          <> T.unpack paramName
          <> " of operation "
          <> T.unpack operationKey

mkSchemaTypeMap :: T.Text -> OA.Schema -> CGU.CodeGen CGU.TypeMap
mkSchemaTypeMap schemaKey schema = do
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
        , CGU.codeGenTypeSchemaInfo = schemaInfo
        , CGU.codeGenTypeDescription = OA._schemaDescription schema
        , CGU.codeGenTypeDataFormat = dataFormat
        }

  pure $ Map.singleton schemaKey (CGU.CodeGenItemType codeGenType) <> inlinedTypes

mkOpenApiDataFormat :: T.Text -> OA.Schema -> CGU.CodeGen (CGU.TypeMap, CGU.CodeGenDataFormat)
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
      let
        toText value =
          case value of
            Aeson.String text -> pure (Just text)
            Aeson.Null ->
              case OA._schemaNullable schema of
                Just True -> pure Nothing
                _ -> CGU.codeGenError "null listed as enum value in a non-nullable schema"
            _ -> CGU.codeGenError "Non-string value found for enum"
      in
        fmap (CGU.enumFormat . catMaybes) (traverse toText enumValues)
    Nothing ->
      pure $
        case OA._schemaFormat schema of
          Just "date" -> CGU.dayFormat
          Just "date-time" -> CGU.utcTimeFormat
          _ -> CGU.textFormat

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
  CGU.CodeGen (CGU.TypeMap, CGU.CodeGenDataFormat)
mkOpenApiObjectFormat schemaKey schema = do
  let
    requiredParams =
      OA._schemaRequired schema

  (typeMaps, fields) <-
    fmap unzip
      . traverse (uncurry $ propertyToCodeGenField schemaKey requiredParams)
      . filter (\(prop, _) -> not (prop `elem` unsupportedProperties))
      . IOHM.toList
      . OA._schemaProperties
      $ schema

  pure (Map.unions typeMaps, CGU.CodeGenObject fields)

unsupportedProperties :: [T.Text]
unsupportedProperties =
  [ "_links"
  ]

mkOpenApiArrayFormat ::
  T.Text ->
  OA.Schema ->
  CGU.CodeGen (CGU.TypeMap, CGU.CodeGenDataFormat)
mkOpenApiArrayFormat schemaKey schema = do
  fmap (fmap CGU.CodeGenArray) $
    schemaArrayItemsToFieldType
      schemaKey
      schemaKey
      (OA._schemaItems schema)

propertyToCodeGenField ::
  T.Text ->
  [OA.ParamName] ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen (CGU.TypeMap, CGU.CodeGenObjectField)
propertyToCodeGenField parentSchemaKey requiredParams name schemaRef = do
  (typeMap, codeGenFieldType) <-
    schemaRefToFieldType parentSchemaKey name schemaRef

  let
    field =
      CGU.CodeGenObjectField
        { CGU.codeGenFieldName = name
        , CGU.codeGenFieldRequired = name `elem` requiredParams
        , CGU.codeGenFieldType = codeGenFieldType
        }

  pure (typeMap, field)

schemaRefToFieldType ::
  T.Text ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen (CGU.TypeMap, CGU.CodeGenObjectFieldType)
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
                fieldName
                (OA._schemaItems inline)
        _ -> do
          let
            key =
              parentKey <> "." <> fieldName

            childRef =
              CGU.TypeReference key

          typeMap <- mkSchemaTypeMap key inline
          pure (typeMap, childRef)

schemaArrayItemsToFieldType ::
  T.Text ->
  OA.ParamName ->
  Maybe OA.OpenApiItems ->
  CGU.CodeGen (CGU.TypeMap, CGU.CodeGenObjectFieldType)
schemaArrayItemsToFieldType parentKey fieldName arrayItems =
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

        schemaTypeInfo <- CGU.inferSchemaInfoForInputName key

        let
          typeMap =
            Map.singleton key $
              CGU.CodeGenItemType $
                CGU.CodeGenType
                  { CGU.codeGenTypeOriginalName = key
                  , CGU.codeGenTypeSchemaInfo = schemaTypeInfo
                  , CGU.codeGenTypeDescription = Nothing
                  , CGU.codeGenTypeDataFormat = CGU.textFormat
                  }

        pure (typeMap, fieldType)
      Just (OA.OpenApiItemsArray _itemSchemaRefs) ->
        fieldError "Heterogeneous arrays are not supported"
      Nothing ->
        fieldError "Array schema found with no item schema"
