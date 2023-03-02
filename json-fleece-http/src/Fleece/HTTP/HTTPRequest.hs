module Fleece.HTTP.HTTPRequest
  ( httpRequest
  , httpRequestThrow
  , httpRequestHandleResult
  , StatusResult (ExpectedStatus, UnexpectedStatus)
  ) where

import qualified Control.Exception as Exc
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes

import Fleece.HTTP.RequestDefinition
  ( RequestDefinition
  , StatusRange (ClientError, Informational, Redirect, ServerError, Status, Success)
  , checkStatus
  , encodeRequestBody
  , parseHTTPResponse
  , requestMethod
  , requestSchema
  , requestSchemaHeaders
  , responseSchemaRequestHeaders
  , responseSchemas
  )

data StatusResult unexpectedStatusBody err response
  = ExpectedStatus HTTP.Request (HTTP.Response ()) (Either err response)
  | UnexpectedStatus HTTP.Request (HTTP.Response unexpectedStatusBody)

httpRequestThrow ::
  Exc.Exception err =>
  RequestDefinition err request response ->
  request ->
  HTTP.Request ->
  HTTP.Manager ->
  IO response
httpRequestThrow =
  let
    throwErrors statusResult =
      case statusResult of
        ExpectedStatus _request _response (Right result) ->
          pure result
        ExpectedStatus _request _response (Left err) ->
          Exc.throwIO err
        UnexpectedStatus request response -> do
          chunk <- HTTP.brReadSome (HTTP.responseBody response) 1024

          Exc.throwIO
            . HTTP.HttpExceptionRequest request
            . HTTP.StatusCodeException (removeBody response)
            . LBS.toStrict
            $ chunk
  in
    httpRequestHandleResult throwErrors

httpRequest ::
  RequestDefinition err request response ->
  request ->
  HTTP.Request ->
  HTTP.Manager ->
  IO (StatusResult () err response)
httpRequest =
  let
    removeUnexpectedBody statusResult =
      pure $
        case statusResult of
          ExpectedStatus request response decodingResult ->
            ExpectedStatus request response decodingResult
          UnexpectedStatus request response ->
            UnexpectedStatus request (removeBody response)
  in
    httpRequestHandleResult removeUnexpectedBody

httpRequestHandleResult ::
  (StatusResult HTTP.BodyReader err response -> IO result) ->
  RequestDefinition err request response ->
  request ->
  HTTP.Request ->
  HTTP.Manager ->
  IO result
httpRequestHandleResult handleResult definition requestValue incompleteRequest manager =
  let
    rqSchema =
      requestSchema definition

    rspSchemas =
      responseSchemas definition

    body =
      encodeRequestBody rqSchema requestValue

    responseSchemaHeaders (range, schema) =
      if includeHeadersInRequest range
        then responseSchemaRequestHeaders schema
        else []

    headers =
      concat
        [ HTTP.requestHeaders incompleteRequest
        , requestSchemaHeaders rqSchema
        , foldMap responseSchemaHeaders rspSchemas
        ]

    completeRequest =
      incompleteRequest
        { HTTP.method = requestMethod definition
        , HTTP.requestBody = body
        , HTTP.requestHeaders = headers
        }
  in
    HTTP.withResponse completeRequest manager $ \response -> do
      let
        mbResponseSchema =
          List.find
            (\(range, _schema) -> checkStatus range (HTTP.responseStatus response))
            rspSchemas

      case mbResponseSchema of
        Nothing -> handleResult (UnexpectedStatus completeRequest response)
        Just (_range, schema) -> do
          decodingResult <- parseHTTPResponse schema response
          handleResult $
            ExpectedStatus
              completeRequest
              (removeBody response)
              decodingResult

removeBody :: HTTP.Response a -> HTTP.Response ()
removeBody response =
  response
    { HTTP.responseBody = ()
    }

includeHeadersInRequest :: StatusRange -> Bool
includeHeadersInRequest range =
  case range of
    Status code -> HTTPTypes.statusIsSuccessful (toEnum code)
    Informational -> False
    Success -> True
    Redirect -> False
    ClientError -> False
    ServerError -> False
