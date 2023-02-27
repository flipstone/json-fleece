{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3
  ( generateOpenApiFleeceCode
  , mkSchemaTypeMap
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
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
mkCodeGenTypes =
  fmap Map.unions
    . traverse (uncurry mkSchemaTypeMap)
    . IOHM.toList
    . OA._componentsSchemas
    . OA._openApiComponents

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

  pure $ Map.singleton schemaKey codeGenType <> inlinedTypes

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
        -- TODO this is a placeholder
        let
          key =
            fieldName <> "Item"

          fieldType =
            CGU.TypeReference key

        schemaTypeInfo <- CGU.inferSchemaInfoForInputName key

        let
          typeMap =
            Map.singleton key $
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
