{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.Swagger2
  ( generateSwaggerFleeceCode
  ) where

import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict.InsOrd as IOHM
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
  openApi <- swaggerToOpenApi swagger
  FOA3.generateOpenApiFleeceCode openApi

swaggerToOpenApi :: SW.Swagger -> CGU.CodeGen OA.OpenApi
swaggerToOpenApi swagger = do
  schemas <- traverse swaggerSchemaToOpenApi (SW._swaggerDefinitions swagger)
  paths <- traverse swaggerPathItemToOpenApi (SW._swaggerPaths swagger)

  -- Note: We only fill out the fields that we know
  -- generateOpenApiFleeceCodeUses
  pure $
    OA.OpenApi
      { OA._openApiPaths = paths
      , OA._openApiComponents =
          OA.Components
            { OA._componentsSchemas = schemas
            , OA._componentsResponses = mempty
            , OA._componentsParameters = mempty
            , OA._componentsExamples = mempty
            , OA._componentsRequestBodies = mempty
            , OA._componentsHeaders = mempty
            , OA._componentsSecuritySchemes = mempty
            , OA._componentsLinks = mempty
            , OA._componentsCallbacks = mempty
            }
      , OA._openApiInfo = dummyInfo
      , OA._openApiServers = []
      , OA._openApiSecurity = []
      , OA._openApiTags = mempty
      , OA._openApiExternalDocs = Nothing
      , OA._openApiOpenapi = mempty
      }

swaggerPathItemToOpenApi :: SW.PathItem -> CGU.CodeGen OA.PathItem
swaggerPathItemToOpenApi pathItem = do
  getOp <- traverse swaggerOperationToOpenApi (SW._pathItemGet pathItem)
  putOp <- traverse swaggerOperationToOpenApi (SW._pathItemPut pathItem)
  postOp <- traverse swaggerOperationToOpenApi (SW._pathItemPost pathItem)
  deleteOp <- traverse swaggerOperationToOpenApi (SW._pathItemDelete pathItem)
  optionsOp <- traverse swaggerOperationToOpenApi (SW._pathItemOptions pathItem)
  headOp <- traverse swaggerOperationToOpenApi (SW._pathItemHead pathItem)
  patchOp <- traverse swaggerOperationToOpenApi (SW._pathItemPatch pathItem)
  parameters <- traverse swaggerParamRefToOpenApi (SW._pathItemParameters pathItem)

  let
    (bodyParams, otherParams) = partitionEithers parameters

  case bodyParams of
    [] ->
      pure $
        OA.PathItem
          { OA._pathItemSummary = Nothing
          , OA._pathItemDescription = Nothing
          , OA._pathItemGet = getOp
          , OA._pathItemPut = putOp
          , OA._pathItemPost = postOp
          , OA._pathItemDelete = deleteOp
          , OA._pathItemOptions = optionsOp
          , OA._pathItemHead = headOp
          , OA._pathItemPatch = patchOp
          , OA._pathItemTrace = Nothing
          , OA._pathItemServers = []
          , OA._pathItemParameters = otherParams
          }
    _ ->
      CGU.codeGenError "Found body params in a path item but only expected them to be on Operations."

swaggerOperationToOpenApi :: SW.Operation -> CGU.CodeGen OA.Operation
swaggerOperationToOpenApi operation = do
  responses <- swaggerResponsesToOpenApi (SW._operationResponses operation)
  parameters <- traverse swaggerParamRefToOpenApi (SW._operationParameters operation)

  let
    (bodyParams, otherParams) = partitionEithers parameters

  mbRequestBody <-
    case bodyParams of
      [] -> pure Nothing
      [requestBody] -> pure (Just requestBody)
      (_ : _ : _) -> CGU.codeGenError "Multiple body params found on operation."

  pure $
    OA.Operation
      { OA._operationTags = SW._operationTags operation
      , OA._operationSummary = SW._operationSummary operation
      , OA._operationDescription = SW._operationDescription operation
      , OA._operationExternalDocs = fmap swaggerExternalDocsToOpenApi (SW._operationExternalDocs operation)
      , OA._operationOperationId = SW._operationOperationId operation
      , OA._operationParameters = otherParams
      , OA._operationRequestBody = fmap OA.Inline mbRequestBody
      , OA._operationResponses = responses
      , OA._operationCallbacks = mempty
      , OA._operationDeprecated = SW._operationDeprecated operation
      , OA._operationSecurity = coerce (SW._operationSecurity operation)
      , OA._operationServers = []
      }

swaggerResponsesToOpenApi :: SW.Responses -> CGU.CodeGen OA.Responses
swaggerResponsesToOpenApi responses = do
  responsesDefault <- traverse swaggerResponseRefToOpenApi (SW._responsesDefault responses)
  responsesResponses <- traverse swaggerResponseRefToOpenApi (SW._responsesResponses responses)
  pure $
    OA.Responses
      { OA._responsesDefault = responsesDefault
      , OA._responsesResponses = responsesResponses
      }

swaggerResponseRefToOpenApi :: SW.Referenced SW.Response -> CGU.CodeGen (OA.Referenced OA.Response)
swaggerResponseRefToOpenApi =
  traverseRef swaggerResponseToOpenApi

swaggerResponseToOpenApi :: SW.Response -> CGU.CodeGen OA.Response
swaggerResponseToOpenApi response = do
  content <-
    case SW._responseSchema response of
      Nothing -> pure mempty
      Just schema -> do
        openApiSchema <- swaggerSchemaRefToOpenApi schema

        let
          mto =
            OA.MediaTypeObject
              { OA._mediaTypeObjectSchema = Just openApiSchema
              , OA._mediaTypeObjectExample = Nothing
              , OA._mediaTypeObjectExamples = mempty
              , OA._mediaTypeObjectEncoding = mempty
              }

        -- This should be actually be sourced in some way from the produces
        -- attribute of the operation, but since we're only targeting JSON at
        -- the moment we simply hard code the mime type here
        pure (IOHM.singleton "application/json" mto)

  headers <- traverse swaggerHeaderToOpenApi (SW._responseHeaders response)

  pure $
    OA.Response
      { OA._responseDescription = SW._responseDescription response
      , OA._responseContent = content
      , OA._responseHeaders = headers
      , OA._responseLinks = mempty
      }

swaggerHeaderToOpenApi :: SW.Header -> CGU.CodeGen (OA.Referenced OA.Header)
swaggerHeaderToOpenApi header = do
  schema <- swaggerHeaderSchemaToOpenApi (SW._headerParamSchema header)
  pure $
    OA.Inline $
      OA.Header
        { OA._headerDescription = SW._headerDescription header
        , OA._headerSchema = Just schema
        , OA._headerRequired = Nothing
        , OA._headerDeprecated = Nothing
        , OA._headerAllowEmptyValue = Nothing
        , OA._headerExplode = Nothing
        , OA._headerExample = Nothing
        , OA._headerExamples = mempty
        }

swaggerHeaderItemsToOpenApi ::
  SW.SwaggerItems ('SWI.SwaggerKindNormal SW.Header) ->
  CGU.CodeGen OA.OpenApiItems
swaggerHeaderItemsToOpenApi swaggerItems =
  case swaggerItems of
    SW.SwaggerItemsPrimitive _collectionFormat _paramSchema ->
      CGU.codeGenError "Array items not supported in header"

swaggerHeaderSchemaToOpenApi ::
  SW.ParamSchema ('SWI.SwaggerKindNormal SW.Header) ->
  CGU.CodeGen (OA.Referenced OA.Schema)
swaggerHeaderSchemaToOpenApi paramSchema = do
  openApiItems <-
    traverse swaggerHeaderItemsToOpenApi (SW._paramSchemaItems paramSchema)

  pure $
    OA.Inline $
      OA.Schema
        { OA._schemaTitle = Nothing
        , OA._schemaDescription = Nothing
        , OA._schemaRequired = []
        , OA._schemaNullable = Nothing
        , OA._schemaAllOf = Nothing
        , OA._schemaOneOf = Nothing
        , OA._schemaNot = Nothing
        , OA._schemaAnyOf = Nothing
        , OA._schemaProperties = mempty
        , OA._schemaAdditionalProperties = Nothing
        , OA._schemaDiscriminator = Nothing
        , OA._schemaReadOnly = Nothing
        , OA._schemaWriteOnly = Nothing
        , OA._schemaXml = Nothing
        , OA._schemaExternalDocs = Nothing
        , OA._schemaExample = Nothing
        , OA._schemaDeprecated = Nothing
        , OA._schemaMaxProperties = Nothing
        , OA._schemaMinProperties = Nothing
        , OA._schemaDefault = SW._paramSchemaDefault paramSchema
        , OA._schemaType = fmap swaggerHeaderTypeToOpenApi (SW._paramSchemaType paramSchema)
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

swaggerParamRefToOpenApi ::
  SW.Referenced SW.Param ->
  CGU.CodeGen
    ( Either
        OA.RequestBody
        (OA.Referenced OA.Param)
    )
swaggerParamRefToOpenApi paramRef =
  case paramRef of
    SW.Ref (SW.Reference refKey) -> pure (Right (OA.Ref (OA.Reference refKey)))
    SW.Inline param -> fmap (fmap OA.Inline) (swaggerParamToOpenApi param)

-- Returns a left schema if the param was the body param and a right param
-- otherwise
swaggerParamToOpenApi :: SW.Param -> CGU.CodeGen (Either OA.RequestBody OA.Param)
swaggerParamToOpenApi param = do
  case SW._paramSchema param of
    SW.ParamBody schemaRef -> do
      openApiSchemaRef <- swaggerSchemaRefToOpenApi schemaRef

      let
        mto =
          OA.MediaTypeObject
            { OA._mediaTypeObjectSchema = Just openApiSchemaRef
            , OA._mediaTypeObjectExample = Nothing
            , OA._mediaTypeObjectExamples = mempty
            , OA._mediaTypeObjectEncoding = mempty
            }

        -- We're only concerned with JSON, so we assume the request body should
        -- be JSON. In principle we should use the consumes property of the
        -- operation.
        content =
          IOHM.singleton "application/json" mto

      pure . Left $
        OA.RequestBody
          { OA._requestBodyDescription = SW._paramDescription param
          , OA._requestBodyContent = content
          , OA._requestBodyRequired = SW._paramRequired param
          }
    SW.ParamOther otherSchema -> do
      schema <- swaggerOtherSchemaToOpenApi (SW._paramOtherSchemaParamSchema otherSchema)
      paramLocation <- swaggerParamLocationToOpenApi (SW._paramOtherSchemaIn otherSchema)
      pure $
        Right $
          OA.Param
            { OA._paramName = SW._paramName param
            , OA._paramDescription = SW._paramDescription param
            , OA._paramRequired = SW._paramRequired param
            , OA._paramDeprecated = Nothing
            , OA._paramIn = paramLocation
            , OA._paramAllowEmptyValue = SW._paramOtherSchemaAllowEmptyValue otherSchema
            , OA._paramAllowReserved = Nothing
            , OA._paramSchema = Just schema
            , OA._paramStyle = Nothing
            , OA._paramExplode = Nothing
            , OA._paramExample = Nothing
            , OA._paramExamples = mempty
            }

swaggerParamLocationToOpenApi :: SW.ParamLocation -> CGU.CodeGen OA.ParamLocation
swaggerParamLocationToOpenApi paramLoc =
  case paramLoc of
    SW.ParamQuery -> pure OA.ParamQuery
    SW.ParamHeader -> pure OA.ParamHeader
    SW.ParamPath -> pure OA.ParamPath
    SW.ParamFormData -> CGU.codeGenError "Form data params not yet supported."

swaggerOtherSchemaToOpenApi ::
  SW.ParamSchema 'SWI.SwaggerKindParamOtherSchema ->
  CGU.CodeGen (OA.Referenced OA.Schema)
swaggerOtherSchemaToOpenApi paramSchema = do
  openApiItems <-
    traverse swaggerOtherItemsToOpenApi (SW._paramSchemaItems paramSchema)

  schemaType <-
    traverse swaggerOtherTypeToOpenApi (SW._paramSchemaType paramSchema)

  pure $
    OA.Inline $
      OA.Schema
        { OA._schemaTitle = Nothing
        , OA._schemaDescription = Nothing
        , OA._schemaRequired = []
        , OA._schemaNullable = Nothing
        , OA._schemaAllOf = Nothing
        , OA._schemaOneOf = Nothing
        , OA._schemaNot = Nothing
        , OA._schemaAnyOf = Nothing
        , OA._schemaProperties = mempty
        , OA._schemaAdditionalProperties = Nothing
        , OA._schemaDiscriminator = Nothing
        , OA._schemaReadOnly = Nothing
        , OA._schemaWriteOnly = Nothing
        , OA._schemaXml = Nothing
        , OA._schemaExternalDocs = Nothing
        , OA._schemaExample = Nothing
        , OA._schemaDeprecated = Nothing
        , OA._schemaMaxProperties = Nothing
        , OA._schemaMinProperties = Nothing
        , OA._schemaDefault = SW._paramSchemaDefault paramSchema
        , OA._schemaType = schemaType
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

swaggerOtherItemsToOpenApi ::
  SW.SwaggerItems 'SWI.SwaggerKindParamOtherSchema ->
  CGU.CodeGen OA.OpenApiItems
swaggerOtherItemsToOpenApi swaggerItems =
  case swaggerItems of
    SW.SwaggerItemsPrimitive _collectionFormat _paramSchema ->
      CGU.codeGenError "Array items not supported in params"

swaggerSchemaToOpenApi :: SW.Schema -> CGU.CodeGen OA.Schema
swaggerSchemaToOpenApi schema = do
  let
    paramSchema =
      SW._schemaParamSchema schema

  openApiAllOf <-
    traverse (traverse swaggerSchemaRefToOpenApi) (SW._schemaAllOf schema)

  openApiItems <-
    traverse swaggerSchemaItemsToOpenApi (SW._paramSchemaItems paramSchema)

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
      , OA._schemaType = fmap swaggerSchemaTypeToOpenApi (SW._paramSchemaType paramSchema)
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

swaggerSchemaTypeToOpenApi :: SW.SwaggerType 'SWI.SwaggerKindSchema -> OA.OpenApiType
swaggerSchemaTypeToOpenApi swaggerType =
  case swaggerType of
    SW.SwaggerString -> OA.OpenApiString
    SW.SwaggerNumber -> OA.OpenApiNumber
    SW.SwaggerInteger -> OA.OpenApiInteger
    SW.SwaggerBoolean -> OA.OpenApiBoolean
    SW.SwaggerArray -> OA.OpenApiArray
    SW.SwaggerNull -> OA.OpenApiNull
    SW.SwaggerObject -> OA.OpenApiObject

swaggerHeaderTypeToOpenApi ::
  SW.SwaggerType ('SWI.SwaggerKindNormal SW.Header) ->
  OA.OpenApiType
swaggerHeaderTypeToOpenApi swaggerType =
  case swaggerType of
    SW.SwaggerString -> OA.OpenApiString
    SW.SwaggerNumber -> OA.OpenApiNumber
    SW.SwaggerInteger -> OA.OpenApiInteger
    SW.SwaggerBoolean -> OA.OpenApiBoolean
    SW.SwaggerArray -> OA.OpenApiArray

swaggerOtherTypeToOpenApi ::
  SW.SwaggerType 'SWI.SwaggerKindParamOtherSchema ->
  CGU.CodeGen OA.OpenApiType
swaggerOtherTypeToOpenApi swaggerType =
  case swaggerType of
    SW.SwaggerString -> pure OA.OpenApiString
    SW.SwaggerNumber -> pure OA.OpenApiNumber
    SW.SwaggerInteger -> pure OA.OpenApiInteger
    SW.SwaggerBoolean -> pure OA.OpenApiBoolean
    SW.SwaggerArray -> pure OA.OpenApiArray
    SW.SwaggerFile -> CGU.codeGenError "File params are not yet supported."

swaggerSchemaItemsToOpenApi :: SW.SwaggerItems 'SWI.SwaggerKindSchema -> CGU.CodeGen OA.OpenApiItems
swaggerSchemaItemsToOpenApi swaggerItems =
  case swaggerItems of
    SW.SwaggerItemsPrimitive _collectionFormat _paramSchema ->
      CGU.codeGenError "Primitive array items found in schema description, but should only be used for query params, headers and path pieces"
    SW.SwaggerItemsObject schemaRef ->
      fmap OA.OpenApiItemsObject (swaggerSchemaRefToOpenApi schemaRef)
    SW.SwaggerItemsArray schemaRefs ->
      fmap OA.OpenApiItemsArray (traverse swaggerSchemaRefToOpenApi schemaRefs)

swaggerSchemaRefToOpenApi :: SW.Referenced SW.Schema -> CGU.CodeGen (OA.Referenced OA.Schema)
swaggerSchemaRefToOpenApi =
  traverseRef swaggerSchemaToOpenApi

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

dummyInfo :: OA.Info
dummyInfo =
  OA.Info
    { OA._infoTitle = "Swagger2 conversion dummy"
    , OA._infoDescription = Nothing
    , OA._infoTermsOfService = Nothing
    , OA._infoContact = Nothing
    , OA._infoLicense = Nothing
    , OA._infoVersion = "dummy-version"
    }

traverseRef :: (Applicative f) => (a -> f b) -> SW.Referenced a -> f (OA.Referenced b)
traverseRef f ref =
  case ref of
    SW.Ref (SW.Reference refKey) -> pure (OA.Ref (OA.Reference refKey))
    SW.Inline item -> fmap OA.Inline (f item)
