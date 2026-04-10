{-# LANGUAGE TypeFamilies #-}

{- | Integration between Fleece\/Aeson and the Beeline HTTP client library.
Provides content type instances for encoding and decoding JSON
request\/response bodies.
-}
module Fleece.Aeson.Beeline
  ( JSON (JSON)
  , JSONDecodingError (..)
  ) where

import qualified Beeline.HTTP.Client as BHC
import qualified Control.Exception as Exc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as HTTP

import qualified Fleece.Aeson as FA
import qualified Fleece.Core as FC

{- | A content type tag for JSON. Used with Beeline's content type
encoding\/decoding infrastructure to handle @application\/json@ request and
response bodies.
-}
data JSON
  = JSON

-- | An error that occurs when decoding a JSON response body fails.
data JSONDecodingError = JSONDecodingError
  { jsonDecodingErrorBytes :: BS.ByteString
  -- ^ The raw bytes that failed to decode.
  , jsonDecodingErrorMessage :: String
  -- ^ The error message describing the decoding failure.
  }
  deriving (Show)

instance Exc.Exception JSONDecodingError

instance BHC.ContentTypeEncoder JSON where
  type EncodeSchema JSON = FC.Schema FA.Encoder

  toRequestContentType JSON _ =
    jsonContentType

  toRequestBody JSON schema =
    HTTP.RequestBodyLBS . FA.encode schema

instance BHC.ContentTypeDecoder JSON where
  type DecodeSchema JSON = FC.Schema FA.Decoder
  type DecodingError JSON = JSONDecodingError

  toResponseContentType JSON _ =
    jsonContentType

  parseResponse JSON schema reader = do
    chunks <- HTTP.brConsume reader
    let
      bytes = LBS.fromChunks chunks
    pure $
      case FA.decode schema bytes of
        Left err -> Left JSONDecodingError {jsonDecodingErrorBytes = BS.toStrict bytes, jsonDecodingErrorMessage = err}
        Right response -> Right response

jsonContentType :: BS.ByteString
jsonContentType =
  BS8.pack "application/json"
