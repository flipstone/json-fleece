{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3.Traversal
  ( traverseOpenApiSchemas
  ) where

#if MIN_VERSION_base(4,18,0)
#else
import Control.Applicative (liftA2)
#endif
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.OpenApi as OA
import qualified Data.Text as T

{- | Traversal of all of the 'OA.Referenced OA.Schema's in the 'OA.OpenApi'.
The traversal is shallow. It does not recursively traverse the 'OA.Schema's found inside of 'OA.Schema'.
The 'T.Text' argument identifies the location of the schema, generally for use in error messages.
-}
traverseOpenApiSchemas ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  OA.OpenApi ->
  f OA.OpenApi
traverseOpenApiSchemas fn openApi =
  let
    mkOpenApi schemas paths =
      openApi
        { OA._openApiComponents = schemas
        , OA._openApiPaths = paths
        }
  in
    liftA2
      mkOpenApi
      (traverseComponentsSchemas fn (OA._openApiComponents openApi))
      (IOHM.unorderedTraverseWithKey (traversePathItemSchemas fn . T.pack) (OA._openApiPaths openApi))

-- | Traverses the 'OA.Referenced OA.Schema's in 'OA.Components', treating those found in 'OA._componentsSchemas' as 'OA.Inline'.
traverseComponentsSchemas ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  OA.Components ->
  f OA.Components
traverseComponentsSchemas fn components =
  let
    getInline ref =
      case ref of
        OA.Ref _ref -> Nothing
        OA.Inline a -> Just a
    mkComponents schemas responses params requestBodies headers callbacks =
      components
        { OA._componentsSchemas = IOHM.mapMaybe getInline schemas
        , OA._componentsResponses = responses
        , OA._componentsParameters = params
        , OA._componentsRequestBodies = requestBodies
        , OA._componentsHeaders = headers
        , OA._componentsCallbacks = callbacks
        }
  in
    mkComponents
      <$> IOHM.unorderedTraverseWithKey (\k -> fn k . OA.Inline) (OA._componentsSchemas components)
      <*> IOHM.unorderedTraverseWithKey (traverseResponseSchemas fn) (OA._componentsResponses components)
      <*> IOHM.unorderedTraverseWithKey (traverseParamSchemas . fn) (OA._componentsParameters components)
      <*> IOHM.unorderedTraverseWithKey (traverseRequestBodySchemas fn) (OA._componentsRequestBodies components)
      <*> IOHM.unorderedTraverseWithKey (traverseHeaderSchemas . fn) (OA._componentsHeaders components)
      <*> IOHM.unorderedTraverseWithKey (traverseCallbackSchema fn) (OA._componentsCallbacks components)

traverseResponseSchemas ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  T.Text ->
  OA.Response ->
  f OA.Response
traverseResponseSchemas fn responseKey response =
  let
    mkMediaTypeObjectKey child = responseKey <> "." <> T.pack (show child)
    mkResponseKey child = responseKey <> "." <> child
    mkResponse content headers =
      response
        { OA._responseContent = content
        , OA._responseHeaders = headers
        }
  in
    liftA2
      mkResponse
      (IOHM.unorderedTraverseWithKey (\k -> traverseMediaTypeObjectSchemas (fn $ mkMediaTypeObjectKey k)) (OA._responseContent response))
      (IOHM.unorderedTraverseWithKey (\k v -> traverseInlineReferenced (traverseHeaderSchemas (fn $ mkResponseKey k)) v) (OA._responseHeaders response))

traverseMediaTypeObjectSchemas ::
  Applicative f =>
  (OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  OA.MediaTypeObject ->
  f OA.MediaTypeObject
traverseMediaTypeObjectSchemas fn mto =
  let
    mkMediaTypeObject schema =
      mto
        { OA._mediaTypeObjectSchema = Just schema
        }
  in
    case OA._mediaTypeObjectSchema mto of
      Just schema -> mkMediaTypeObject <$> fn schema
      Nothing -> pure mto

traverseParamSchemas ::
  Applicative f =>
  (OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  OA.Param ->
  f OA.Param
traverseParamSchemas fn param =
  let
    mkParam schema =
      param
        { OA._paramSchema = Just schema
        }
  in
    case OA._paramSchema param of
      Just schema -> mkParam <$> fn schema
      Nothing -> pure param

traverseRequestBodySchemas ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  T.Text ->
  OA.RequestBody ->
  f OA.RequestBody
traverseRequestBodySchemas fn requestBodyKey requestBody =
  let
    mkMediaTypeObjectKey child = requestBodyKey <> "." <> T.pack (show child)
    mkRequestBodies content =
      requestBody
        { OA._requestBodyContent = content
        }
  in
    mkRequestBodies
      <$> IOHM.unorderedTraverseWithKey (\k -> traverseMediaTypeObjectSchemas (fn $ mkMediaTypeObjectKey k)) (OA._requestBodyContent requestBody)

traverseHeaderSchemas ::
  Applicative f =>
  (OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  OA.Header ->
  f OA.Header
traverseHeaderSchemas fn header =
  let
    mkHeader schema =
      header
        { OA._headerSchema = Just schema
        }
  in
    case OA._headerSchema header of
      Just schema -> mkHeader <$> fn schema
      Nothing -> pure header

traverseCallbackSchema ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  T.Text ->
  OA.Callback ->
  f OA.Callback
traverseCallbackSchema fn callbackKey (OA.Callback callback) =
  let
    mkCallbackKey child = callbackKey <> "." <> child
  in
    OA.Callback
      <$> IOHM.unorderedTraverseWithKey (\k -> traversePathItemSchemas fn $ mkCallbackKey k) callback

traversePathItemSchemas ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  T.Text ->
  OA.PathItem ->
  f OA.PathItem
traversePathItemSchemas fn fp pathItem =
  let
    mkPathItem get put post delete options headOp patch trace params =
      pathItem
        { OA._pathItemGet = get
        , OA._pathItemPut = put
        , OA._pathItemPost = post
        , OA._pathItemDelete = delete
        , OA._pathItemOptions = options
        , OA._pathItemHead = headOp
        , OA._pathItemPatch = patch
        , OA._pathItemTrace = trace
        , OA._pathItemParameters = params
        }
  in
    mkPathItem
      <$> traverse (traverseOperationSchemas fn fp) (OA._pathItemGet pathItem)
      <*> traverse (traverseOperationSchemas fn fp) (OA._pathItemPut pathItem)
      <*> traverse (traverseOperationSchemas fn fp) (OA._pathItemPost pathItem)
      <*> traverse (traverseOperationSchemas fn fp) (OA._pathItemDelete pathItem)
      <*> traverse (traverseOperationSchemas fn fp) (OA._pathItemOptions pathItem)
      <*> traverse (traverseOperationSchemas fn fp) (OA._pathItemHead pathItem)
      <*> traverse (traverseOperationSchemas fn fp) (OA._pathItemPatch pathItem)
      <*> traverse (traverseOperationSchemas fn fp) (OA._pathItemTrace pathItem)
      <*> traverse (traverseInlineReferenced (traverseParamSchemas $ fn fp)) (OA._pathItemParameters pathItem)

traverseInlineReferenced ::
  Applicative f =>
  (a -> f a) ->
  OA.Referenced a ->
  f (OA.Referenced a)
traverseInlineReferenced fn ref =
  case ref of
    OA.Ref _ref -> pure ref
    OA.Inline a -> OA.Inline <$> fn a

traverseOperationSchemas ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  T.Text ->
  OA.Operation ->
  f OA.Operation
traverseOperationSchemas fn operationKey operation =
  let
    mkCallbackKey child = operationKey <> "." <> child
    mkOperation params requestBody responses callbacks =
      operation
        { OA._operationParameters = params
        , OA._operationRequestBody = requestBody
        , OA._operationResponses = responses
        , OA._operationCallbacks = callbacks
        }
  in
    mkOperation
      <$> traverse (traverseInlineReferenced (traverseParamSchemas $ fn operationKey)) (OA._operationParameters operation)
      <*> traverse (traverseInlineReferenced (traverseRequestBodySchemas fn operationKey)) (OA._operationRequestBody operation)
      <*> traverseResponsesSchemas fn operationKey (OA._operationResponses operation)
      <*> IOHM.unorderedTraverseWithKey (\k v -> traverseInlineReferenced (traverseCallbackSchema fn $ mkCallbackKey k) v) (OA._operationCallbacks operation)

traverseResponsesSchemas ::
  Applicative f =>
  (T.Text -> OA.Referenced OA.Schema -> f (OA.Referenced OA.Schema)) ->
  T.Text ->
  OA.Responses ->
  f OA.Responses
traverseResponsesSchemas fn operationKey responses =
  let
    mkResponseKey k = operationKey <> "." <> T.pack (show k)
    mkResponses def res =
      responses
        { OA._responsesDefault = def
        , OA._responsesResponses = res
        }
  in
    liftA2
      mkResponses
      (traverse (traverseInlineReferenced (traverseResponseSchemas fn operationKey)) (OA._responsesDefault responses))
      (IOHM.unorderedTraverseWithKey (\k -> traverseInlineReferenced (traverseResponseSchemas fn $ mkResponseKey k)) (OA._responsesResponses responses))
