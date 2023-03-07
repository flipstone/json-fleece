{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3
  ( generateOpenApiFleeceCode
  , mkSchemaMap
  , SchemaMap
  , SchemaEntry (..)
  ) where

import Control.Monad ((<=<))
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

    paramModules =
      Map.fromList
        . map mkParamEntry
        . Map.toList
        $ params

  pure $
    Map.singleton operationKey (CGU.CodeGenItemOperation codeGenOperation)
      <> paramModules

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
      OA.Ref _ -> CGU.codeGenError "Param refs not yet implemeted"
      OA.Inline param -> pure param

  let
    paramName =
      OA._paramName param

  (moduleName, defaultParamTypeName) <-
    CGU.inferTypeForInputName CGU.Operation (operationKey <> "." <> paramName)

  case OA._paramSchema param of
    Just schemaRef -> do
      (mbTypeName, paramType) <-
        schemaRefToParamType
          schemaMap
          paramName
          (OA._paramIn param)
          operationKey
          schemaRef

      let
        paramTypeName =
          case mbTypeName of
            Nothing -> defaultParamTypeName
            Just resolvedName -> resolvedName

      pure
        CGU.CodeGenOperationParam
          { CGU.codeGenOperationParamName = paramName
          , CGU.codeGenOperationParamModuleName = moduleName
          , CGU.codeGenOperationParamTypeName = paramTypeName
          , CGU.codeGenOperationParamType = paramType
          , CGU.codeGenOperationParamDefName =
              HC.toVarName
                moduleName
                (Just (HC.typeNameText paramTypeName))
                "paramDef"
          }
    Nothing ->
      CGU.codeGenError $
        "No schema found for param "
          <> T.unpack paramName
          <> " of operation "
          <> T.unpack operationKey

schemaRefToParamType ::
  SchemaMap ->
  T.Text ->
  OA.ParamLocation ->
  T.Text ->
  OA.Referenced OA.Schema ->
  CGU.CodeGen (Maybe HC.TypeName, CGU.OperationParamType)
schemaRefToParamType schemaMap paramName paramLocation operationKey schemaRef =
  case schemaRef of
    OA.Inline schema -> do
      schemaTypeToParamType
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

          (_resolvedType, paramType) <-
            schemaTypeToParamType
              schemaMap
              paramName
              paramLocation
              operationKey
              (schemaOpenApiSchema schemaEntry)

          pure (Just (CGU.codeGenTypeName codeGenType), paramType)
        Nothing ->
          CGU.codeGenError $
            "Schema reference "
              <> show refKey
              <> " not found for param "
              <> T.unpack paramName
              <> " of operation "
              <> T.unpack operationKey

schemaTypeToParamType ::
  SchemaMap ->
  T.Text ->
  OA.ParamLocation ->
  T.Text ->
  OA.Schema ->
  CGU.CodeGen (Maybe HC.TypeName, CGU.OperationParamType)
schemaTypeToParamType schemaMap paramName paramLocation operationKey schema =
  case OA._schemaType schema of
    Just OA.OpenApiString ->
      case OA._schemaEnum schema of
        Nothing ->
          pure (Nothing, CGU.ParamTypeString)
        Just enumValues -> do
          let
            rejectNull mbText =
              case mbText of
                Nothing -> CGU.codeGenError "null not supported as enum value in params"
                Just text -> pure text

          enumTexts <-
            traverse (rejectNull <=< enumValueToText schema) enumValues

          pure (Nothing, CGU.ParamTypeEnum enumTexts)
    Just OA.OpenApiBoolean ->
      pure (Nothing, CGU.ParamTypeBoolean)
    Just OA.OpenApiInteger ->
      case OA._schemaFormat schema of
        Just "int8" -> pure (Nothing, CGU.ParamTypeInt8)
        Just "int16" -> pure (Nothing, CGU.ParamTypeInt16)
        Just "int32" -> pure (Nothing, CGU.ParamTypeInt32)
        Just "int64" -> pure (Nothing, CGU.ParamTypeInt64)
        _ -> pure (Nothing, CGU.ParamTypeInteger)
    Just OA.OpenApiArray ->
      case paramLocation of
        OA.ParamQuery ->
          case OA._schemaItems schema of
            Just (OA.OpenApiItemsObject itemSchemaRef) ->
              fmap (fmap CGU.ParamTypeArray) $
                schemaRefToParamType
                  schemaMap
                  paramName
                  paramLocation
                  operationKey
                  itemSchemaRef
            otherItemType ->
              CGU.codeGenError $
                "Unsupported schema array item type found for param "
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
