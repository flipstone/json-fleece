{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.Swagger2
  ( generateSwaggerFleeceCode
  ) where

import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as OA
import qualified Data.Swagger as SW
import qualified Data.Swagger.Internal as SWI
import qualified Data.Text as T

import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.OpenApi3 as FOA3

generateSwaggerFleeceCode ::
  SW.Swagger ->
  CGU.CodeGen CGU.Modules
generateSwaggerFleeceCode swagger = do
  typeMap <- mkCodeGenTypes swagger
  CGU.generateFleeceCode typeMap

mkCodeGenTypes :: SW.Swagger -> CGU.CodeGen CGU.TypeMap
mkCodeGenTypes =
  fmap Map.unions
    . traverse (uncurry mkSchemaTypeMap)
    . IOHM.toList
    . SW._swaggerDefinitions

mkSchemaTypeMap :: T.Text -> SW.Schema -> CGU.CodeGen CGU.TypeMap
mkSchemaTypeMap schemaKey schema =
  FOA3.mkSchemaTypeMap schemaKey =<< swaggerSchemaToOpenApi schema

swaggerSchemaToOpenApi :: SW.Schema -> CGU.CodeGen OA.Schema
swaggerSchemaToOpenApi schema = do
  let
    paramSchema =
      SW._schemaParamSchema schema

  openApiAllOf <-
    traverse (traverse swaggerSchemaRefToOpenApi) (SW._schemaAllOf schema)

  openApiItems <-
    traverse swaggerItemsToOpenApi (SW._paramSchemaItems paramSchema)

  openApiProps <-
    traverse swaggerSchemaRefToOpenApi (SW._schemaProperties schema)

  openApiAdditionalProps <-
    traverse swaggerAdditionalPropsToOpenApi (SW._schemaAdditionalProperties schema)

  pure $
    OA.Schema
      { OA._schemaTitle = SW._schemaTitle schema
      , OA._schemaDescription = SW._schemaDescription schema
      , OA._schemaRequired = SW._schemaRequired schema
      , OA._schemaNullable = Nothing
      , OA._schemaAllOf = openApiAllOf
      , OA._schemaOneOf = Nothing
      , OA._schemaNot = Nothing
      , OA._schemaAnyOf = Nothing
      , OA._schemaProperties = openApiProps
      , OA._schemaAdditionalProperties = openApiAdditionalProps
      , OA._schemaDiscriminator = fmap swaggerDiscriminatorToOpenApi (SW._schemaDiscriminator schema)
      , OA._schemaReadOnly = SW._schemaReadOnly schema
      , OA._schemaWriteOnly = Nothing
      , OA._schemaXml = fmap swaggerXmlToOpenApi (SW._schemaXml schema)
      , OA._schemaExternalDocs = fmap swaggerExternalDocsToOpenApi (SW._schemaExternalDocs schema)
      , OA._schemaExample = SW._schemaExample schema
      , OA._schemaDeprecated = Nothing
      , OA._schemaMaxProperties = SW._schemaMaxProperties schema
      , OA._schemaMinProperties = SW._schemaMinProperties schema
      , OA._schemaDefault = SW._paramSchemaDefault paramSchema
      , OA._schemaType = fmap swaggerTypeToOpenApi (SW._paramSchemaType paramSchema)
      , OA._schemaFormat = SW._paramSchemaFormat paramSchema
      , OA._schemaItems = openApiItems
      , OA._schemaMaximum = SW._paramSchemaMaximum paramSchema
      , OA._schemaExclusiveMaximum = SW._paramSchemaExclusiveMaximum paramSchema
      , OA._schemaMinimum = SW._paramSchemaMinimum paramSchema
      , OA._schemaExclusiveMinimum = SW._paramSchemaExclusiveMinimum paramSchema
      , OA._schemaMaxLength = SW._paramSchemaMaxLength paramSchema
      , OA._schemaMinLength = SW._paramSchemaMinLength paramSchema
      , OA._schemaPattern = SW._paramSchemaPattern paramSchema
      , OA._schemaMaxItems = SW._paramSchemaMaxItems paramSchema
      , OA._schemaMinItems = SW._paramSchemaMinItems paramSchema
      , OA._schemaUniqueItems = SW._paramSchemaUniqueItems paramSchema
      , OA._schemaEnum = SW._paramSchemaEnum paramSchema
      , OA._schemaMultipleOf = SW._paramSchemaMultipleOf paramSchema
      }

swaggerTypeToOpenApi :: SW.SwaggerType 'SWI.SwaggerKindSchema -> OA.OpenApiType
swaggerTypeToOpenApi swaggerType =
  case swaggerType of
    SW.SwaggerString -> OA.OpenApiString
    SW.SwaggerNumber -> OA.OpenApiNumber
    SW.SwaggerInteger -> OA.OpenApiInteger
    SW.SwaggerBoolean -> OA.OpenApiBoolean
    SW.SwaggerArray -> OA.OpenApiArray
    SW.SwaggerNull -> OA.OpenApiNull
    SW.SwaggerObject -> OA.OpenApiObject

swaggerItemsToOpenApi :: SW.SwaggerItems 'SWI.SwaggerKindSchema -> CGU.CodeGen OA.OpenApiItems
swaggerItemsToOpenApi swaggerItems =
  case swaggerItems of
    SW.SwaggerItemsPrimitive _collectionFormat _paramSchema ->
      CGU.codeGenError "Primitive array items found in schema description, but should only be used for query params, headers and path pieces"
    SW.SwaggerItemsObject schemaRef ->
      fmap OA.OpenApiItemsObject (swaggerSchemaRefToOpenApi schemaRef)
    SW.SwaggerItemsArray schemaRefs ->
      fmap OA.OpenApiItemsArray (traverse swaggerSchemaRefToOpenApi schemaRefs)

swaggerSchemaRefToOpenApi :: SW.Referenced SW.Schema -> CGU.CodeGen (OA.Referenced OA.Schema)
swaggerSchemaRefToOpenApi swaggerSchemaRef =
  case swaggerSchemaRef of
    SW.Ref (SW.Reference key) -> pure $ OA.Ref (OA.Reference key)
    SW.Inline schema -> fmap OA.Inline (swaggerSchemaToOpenApi schema)

swaggerAdditionalPropsToOpenApi :: SW.AdditionalProperties -> CGU.CodeGen OA.AdditionalProperties
swaggerAdditionalPropsToOpenApi additionalProps =
  case additionalProps of
    SW.AdditionalPropertiesAllowed bool ->
      pure $ OA.AdditionalPropertiesAllowed bool
    SW.AdditionalPropertiesSchema schemaRef ->
      fmap OA.AdditionalPropertiesSchema (swaggerSchemaRefToOpenApi schemaRef)

swaggerDiscriminatorToOpenApi :: T.Text -> OA.Discriminator
swaggerDiscriminatorToOpenApi descriminator =
  OA.Discriminator
    { OA._discriminatorPropertyName = descriminator
    , OA._discriminatorMapping = mempty
    }

swaggerXmlToOpenApi :: SW.Xml -> OA.Xml
swaggerXmlToOpenApi swaggerXml =
  OA.Xml
    { OA._xmlName = SW._xmlName swaggerXml
    , OA._xmlNamespace = SW._xmlNamespace swaggerXml
    , OA._xmlPrefix = SW._xmlPrefix swaggerXml
    , OA._xmlAttribute = SW._xmlAttribute swaggerXml
    , OA._xmlWrapped = SW._xmlWrapped swaggerXml
    }

swaggerExternalDocsToOpenApi :: SW.ExternalDocs -> OA.ExternalDocs
swaggerExternalDocsToOpenApi swaggerDocs =
  OA.ExternalDocs
    { OA._externalDocsDescription = SW._externalDocsDescription swaggerDocs
    , OA._externalDocsUrl = OA.URL . SW.getUrl . SW._externalDocsUrl $ swaggerDocs
    }
