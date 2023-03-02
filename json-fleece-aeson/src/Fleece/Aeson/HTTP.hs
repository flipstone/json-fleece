{-# LANGUAGE TypeFamilies #-}

module Fleece.Aeson.HTTP
  ( Aeson (Aeson)
  ) where

import qualified Data.ByteString.Lazy as LBS
import Fleece.Aeson.Decoder (Decoder, decode)
import Fleece.Aeson.Encoder (Encoder, encode)
import qualified Fleece.HTTP as FH

data Aeson = Aeson

instance FH.HTTPEncoder Aeson where
  type EncodeSchema Aeson = Encoder

  toRequestBody Aeson encoder =
    FH.RequestBodyLBS . encode encoder

instance FH.HTTPDecoder Aeson where
  type DecodeSchema Aeson = Decoder
  type DecodingError Aeson = FH.HTTPResponseDecodingError

  parseResponse Aeson decoder bodyReader = do
    chunks <- FH.brConsume bodyReader
    pure
      . either (Left . FH.HTTPResponseDecodingError) Right
      . decode decoder
      . LBS.fromChunks
      $ chunks
