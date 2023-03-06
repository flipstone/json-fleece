{-# LANGUAGE TypeFamilies #-}

module Fleece.Aeson.Beeline
  ( JSON (JSON)
  ) where

import qualified Beeline.HTTP.Client as BHC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as HTTP

import qualified Fleece.Aeson as FA

{-
  A content type tag for dealing with JSON text
-}
data JSON
  = JSON

instance BHC.ContentTypeEncoder JSON where
  type EncodeSchema JSON = FA.Encoder

  toRequestContentType JSON _ =
    jsonContentType

  toRequestBody JSON schema =
    HTTP.RequestBodyLBS . FA.encode schema

instance BHC.ContentTypeDecoder JSON where
  type DecodeSchema JSON = FA.Decoder
  type DecodingError JSON = BHC.ContentTypeDecodingError

  toResponseContentType JSON _ =
    jsonContentType

  parseResponse JSON schema reader = do
    bytes <- HTTP.brConsume reader
    pure $
      case FA.decode schema (LBS.fromChunks bytes) of
        Left err -> Left (BHC.ContentTypeDecodingError err)
        Right response -> Right response

jsonContentType :: BS.ByteString
jsonContentType =
  BS8.pack "application/json"
