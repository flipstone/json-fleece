{-# LANGUAGE OverloadedStrings #-}

module Fleece.HTTP.RequestDefinition
  ( RequestDefinition
      ( RequestDefinition
      , requestMethod
      , requestSchema
      , responseSchemas
      )
  , RequestSchema
    ( RequestSchema
    , requestSchemaHeaders
    , encodeRequestBody
    )
  , encodeRequestWith
  , noRequestBody
  , ResponseSchema
    ( ResponseSchema
    , responseSchemaRequestHeaders
    , parseHTTPResponse
    )
  , decodeResponseWith
  , StatusRange
    ( Status
    , Informational
    , Success
    , Redirect
    , ClientError
    , ServerError
    )
  , checkStatus
  , noResponseBody
  , getRequest
  , postRequest
  , headRequest
  , putRequest
  , deleteRequest
  , traceRequest
  , connectRequest
  , optionsRequest
  , patchRequest
  , defaultRequestDefinition
  ) where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes

import Fleece.HTTP.HTTPDecoder (DecodeSchema, DecodingError, HTTPDecoder, parseResponse)
import Fleece.HTTP.HTTPEncoder (EncodeSchema, HTTPEncoder, toRequestBody)

data RequestDefinition err request response = RequestDefinition
  { requestMethod :: HTTPTypes.Method
  , requestSchema :: RequestSchema request
  , responseSchemas :: [(StatusRange, ResponseSchema err response)]
  }

data RequestSchema a = RequestSchema
  { requestSchemaHeaders :: [HTTPTypes.Header]
  , encodeRequestBody :: a -> HTTP.RequestBody
  }

noRequestBody :: RequestSchema ()
noRequestBody =
  RequestSchema
    { requestSchemaHeaders = []
    , encodeRequestBody = (\() -> "")
    }

encodeRequestWith :: HTTPEncoder coder => coder -> EncodeSchema coder a -> RequestSchema a
encodeRequestWith coder encoder =
  RequestSchema
    { requestSchemaHeaders = [("Content-Type", "application/json")]
    , encodeRequestBody = toRequestBody coder encoder
    }

data StatusRange
  = Status Int
  | Informational
  | Success
  | Redirect
  | ClientError
  | ServerError

checkStatus :: StatusRange -> HTTPTypes.Status -> Bool
checkStatus range status =
  case range of
    Status code -> HTTPTypes.statusCode status == code
    Informational -> HTTPTypes.statusIsInformational status
    Success -> HTTPTypes.statusIsSuccessful status
    Redirect -> HTTPTypes.statusIsRedirection status
    ClientError -> HTTPTypes.statusIsClientError status
    ServerError -> HTTPTypes.statusIsServerError status

data ResponseSchema err a = ResponseSchema
  { responseSchemaRequestHeaders :: [HTTPTypes.Header]
  , parseHTTPResponse :: HTTP.Response HTTP.BodyReader -> IO (Either err a)
  }

instance Functor (ResponseSchema err) where
  fmap f schema =
    schema
      { parseHTTPResponse = fmap (fmap f) . parseHTTPResponse schema
      }

noResponseBody :: ResponseSchema err ()
noResponseBody =
  ResponseSchema
    { responseSchemaRequestHeaders = []
    , parseHTTPResponse = \_response -> pure (Right ())
    }

decodeResponseWith ::
  HTTPDecoder coder =>
  coder ->
  DecodeSchema coder a ->
  ResponseSchema (DecodingError coder) a
decodeResponseWith coder decoder =
  ResponseSchema
    { responseSchemaRequestHeaders = [("Accept", "application/json")]
    , parseHTTPResponse = parseResponse coder decoder . HTTP.responseBody
    }

defaultRequestDefinition :: HTTPTypes.Method -> RequestDefinition err () ()
defaultRequestDefinition method =
  RequestDefinition
    { requestMethod = method
    , requestSchema = noRequestBody
    , responseSchemas = [(Success, noResponseBody)]
    }

getRequest :: RequestDefinition err () ()
getRequest =
  defaultRequestDefinition HTTPTypes.methodGet

postRequest :: RequestDefinition err () ()
postRequest =
  defaultRequestDefinition HTTPTypes.methodPost

headRequest :: RequestDefinition err () ()
headRequest =
  defaultRequestDefinition HTTPTypes.methodHead

putRequest :: RequestDefinition err () ()
putRequest =
  defaultRequestDefinition HTTPTypes.methodPut

deleteRequest :: RequestDefinition err () ()
deleteRequest =
  defaultRequestDefinition HTTPTypes.methodDelete

traceRequest :: RequestDefinition err () ()
traceRequest =
  defaultRequestDefinition HTTPTypes.methodTrace

connectRequest :: RequestDefinition err () ()
connectRequest =
  defaultRequestDefinition HTTPTypes.methodConnect

optionsRequest :: RequestDefinition err () ()
optionsRequest =
  defaultRequestDefinition HTTPTypes.methodOptions

patchRequest :: RequestDefinition err () ()
patchRequest =
  defaultRequestDefinition HTTPTypes.methodPatch
