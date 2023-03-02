{-# LANGUAGE OverloadedStrings #-}

module HTTPTests
  ( httpTests
  ) where

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exc
import qualified Data.IORef as IORef
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Random as Rand

import qualified Fleece.Aeson as FA
import qualified Fleece.Core as FC
import qualified Fleece.Examples as Examples
import qualified Fleece.HTTP as FH
import Generators (genScientific, genText)

httpTests :: HH.Group
httpTests =
  HH.Group "json-fleece-aeson - http" tests

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_httpGet", prop_httpGet)
  , ("prop_httpPost", prop_httpPost)
  , ("prop_httpPostNoResponse", prop_httpPostNoResponse)
  , ("prop_multiResponse", prop_multiResponse)
  , ("prop_unexpectedStatus", prop_unexpectedStatus)
  , ("prop_decodingFailure", prop_decodingFailure)
  ]

getFooBar :: FH.RequestDefinition FH.HTTPResponseDecodingError () Examples.FooBar
getFooBar =
  FH.getRequest
    { FH.responseSchemas =
        [ (FH.Success, FH.decodeResponseWith FA.Aeson Examples.fooBarSchema)
        ]
    }

prop_httpGet :: HH.Property
prop_httpGet =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        fooBarResponse =
          Examples.FooBar "foo" 1.0

        handleRequest request = do
          body <- Wai.consumeRequestBodyStrict request

          assertLater $ do
            Wai.requestMethod request === HTTPTypes.methodGet
            lookup "Content-Type" (Wai.requestHeaders request) === Nothing
            lookup "Accept" (Wai.requestHeaders request) === Just "application/json"
            body === ""

          pure $
            Wai.responseLBS HTTPTypes.ok200 [] $
              FA.encode Examples.fooBarSchema fooBarResponse

        issueRequest port = do
          let
            request = HTTP.defaultRequest {HTTP.port = port}

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          FH.httpRequestThrow getFooBar () request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === fooBarResponse

postOptional ::
  FH.RequestDefinition
    FH.HTTPResponseDecodingError
    Examples.OptionalField
    Examples.FooBar
postOptional =
  FH.postRequest
    { FH.requestSchema = FH.encodeRequestWith FA.Aeson Examples.optionalFieldSchema
    , FH.responseSchemas =
        [ (FH.Success, FH.decodeResponseWith FA.Aeson Examples.fooBarSchema)
        ]
    }

prop_httpPost :: HH.Property
prop_httpPost =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        optionalFieldRequest =
          Examples.OptionalField (Just "optional")

        fooBarResponse =
          Examples.FooBar "foo" 1.0

        handleRequest request = do
          body <- Wai.consumeRequestBodyStrict request

          assertLater $ do
            Wai.requestMethod request === HTTPTypes.methodPost
            lookup "Content-Type" (Wai.requestHeaders request) === Just "application/json"
            lookup "Content-Length" (Wai.requestHeaders request) === Just "28"
            lookup "Accept" (Wai.requestHeaders request) === Just "application/json"
            FA.decode Examples.optionalFieldSchema body === Right optionalFieldRequest

          pure $
            Wai.responseLBS HTTPTypes.ok200 [] $
              FA.encode Examples.fooBarSchema fooBarResponse

        issueRequest port = do
          let
            request = HTTP.defaultRequest {HTTP.port = port}

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          FH.httpRequestThrow postOptional optionalFieldRequest request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === fooBarResponse

postNoResponse ::
  FH.RequestDefinition
    FH.HTTPResponseDecodingError
    Examples.OptionalField
    ()
postNoResponse =
  FH.postRequest
    { FH.requestSchema = FH.encodeRequestWith FA.Aeson Examples.optionalFieldSchema
    }

prop_httpPostNoResponse :: HH.Property
prop_httpPostNoResponse =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        optionalFieldRequest =
          Examples.OptionalField (Just "optional")

        handleRequest request = do
          body <- Wai.consumeRequestBodyStrict request

          assertLater $ do
            Wai.requestMethod request === HTTPTypes.methodPost
            lookup "Content-Type" (Wai.requestHeaders request) === Just "application/json"
            lookup "Content-Length" (Wai.requestHeaders request) === Just "28"
            lookup "Accept" (Wai.requestHeaders request) === Nothing
            FA.decode Examples.optionalFieldSchema body === Right optionalFieldRequest

          pure $ Wai.responseLBS HTTPTypes.ok200 [] ""

        issueRequest port = do
          let
            request = HTTP.defaultRequest {HTTP.port = port}

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          FH.httpRequestThrow postNoResponse optionalFieldRequest request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)

      response === ()

data MultiStatus
  = Multi200 Examples.FooBar
  | MultiOtherSuccess Examples.NullableField
  | MultiClientError Examples.OptionalField
  deriving (Show, Eq)

multipleResponseCodes ::
  FH.RequestDefinition
    FH.HTTPResponseDecodingError
    ()
    MultiStatus
multipleResponseCodes =
  FH.getRequest
    { FH.responseSchemas =
        [ (FH.Status 200, fmap Multi200 (FH.decodeResponseWith FA.Aeson Examples.fooBarSchema))
        , (FH.Success, fmap MultiOtherSuccess (FH.decodeResponseWith FA.Aeson Examples.nullableFieldSchema))
        , (FH.ClientError, fmap MultiClientError (FH.decodeResponseWith FA.Aeson Examples.optionalFieldSchema))
        ]
    }

prop_multiResponse :: HH.Property
prop_multiResponse =
  HH.property $ do
    expectedResponse <-
      HH.forAll $
        Gen.choice
          [ fmap Multi200 $ (Examples.FooBar <$> genText <*> genScientific)
          , fmap MultiOtherSuccess $ (Examples.NullableField <$> Gen.either (pure FC.Null) genText)
          , fmap MultiClientError $ (Examples.OptionalField <$> Gen.maybe genText)
          ]

    let
      handleRequest _request =
        pure $
          case expectedResponse of
            Multi200 fooBar ->
              Wai.responseLBS HTTPTypes.ok200 [] (FA.encode Examples.fooBarSchema fooBar)
            MultiOtherSuccess nullable ->
              Wai.responseLBS HTTPTypes.created201 [] (FA.encode Examples.nullableFieldSchema nullable)
            MultiClientError optional ->
              Wai.responseLBS HTTPTypes.notFound404 [] (FA.encode Examples.optionalFieldSchema optional)

      issueRequest port = do
        let
          request = HTTP.defaultRequest {HTTP.port = port}

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        FH.httpRequestThrow multipleResponseCodes () request manager

    response <- HH.evalIO (withTestServer handleRequest issueRequest)
    response === expectedResponse

prop_unexpectedStatus :: HH.Property
prop_unexpectedStatus =
  HH.withTests 1 . HH.property $ do
    let
      handleRequest _request =
        pure $ Wai.responseLBS HTTPTypes.notFound404 [] "Not Found"

      issueRequest port = do
        let
          request = HTTP.defaultRequest {HTTP.port = port}

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Exc.try $ FH.httpRequestThrow getFooBar () request manager

    exceptionOrFooBar <- HH.evalIO (withTestServer handleRequest issueRequest)
    case exceptionOrFooBar of
      Left (HTTP.HttpExceptionRequest _request (HTTP.StatusCodeException _response chunk)) ->
        chunk === "Not Found"
      Left err -> do
        HH.annotate "Expected a StatusCodeException, but got:"
        HH.annotateShow err
        HH.failure
      Right _fooBar -> do
        HH.annotate "Expected an HTTPException, but got a valid response instead"
        HH.failure

prop_decodingFailure :: HH.Property
prop_decodingFailure =
  HH.withTests 1 . HH.property $ do
    let
      handleRequest _request =
        pure $ Wai.responseLBS HTTPTypes.ok200 [] "{}"

      issueRequest port = do
        let
          request = HTTP.defaultRequest {HTTP.port = port}

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Exc.try $ FH.httpRequestThrow getFooBar () request manager

    exceptionOrFooBar <- HH.evalIO (withTestServer handleRequest issueRequest)
    case exceptionOrFooBar of
      Left (FH.HTTPResponseDecodingError msg) ->
        msg === "Error in $: key \"foo\" not found"
      Right _fooBar -> do
        HH.annotate "Expected an HTTPException, but got a valid response instead"
        HH.failure

withAssertLater ::
  ((HH.PropertyT IO () -> IO ()) -> HH.PropertyT IO a) ->
  HH.PropertyT IO a
withAssertLater action = do
  serverAssertions <- HH.evalIO (IORef.newIORef [])

  let
    assertLater assertion =
      IORef.modifyIORef serverAssertions (assertion :)

  result <- action assertLater
  assertions <- HH.evalIO (IORef.readIORef serverAssertions)
  sequence_ (reverse assertions)
  pure result

withTestServer :: (Wai.Request -> IO Wai.Response) -> (Warp.Port -> IO a) -> IO a
withTestServer handleRequest issueRequest = do
  startupVar <- MVar.newEmptyMVar
  port <- Rand.randomRIO (10000, 99999)

  let
    serverSettings =
      Warp.setPort port
        . Warp.setBeforeMainLoop (MVar.putMVar startupVar ())
        $ Warp.defaultSettings

    server =
      Warp.runSettings serverSettings $ \request respondWith ->
        respondWith =<< handleRequest request

    waitForServer =
      MVar.readMVar startupVar

  Exc.bracket
    (Conc.forkIO server)
    Conc.killThread
    (\_threadId -> waitForServer >> issueRequest port)
