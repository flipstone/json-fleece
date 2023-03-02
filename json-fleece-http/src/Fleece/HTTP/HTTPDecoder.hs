{-# LANGUAGE TypeFamilies #-}

module Fleece.HTTP.HTTPDecoder
  ( HTTPDecoder (DecodeSchema, DecodingError, parseResponse)
  , HTTPResponseDecodingError (HTTPResponseDecodingError)
  ) where

import qualified Control.Exception as Exc
import Data.Kind (Type)
import qualified Network.HTTP.Client as HTTP

class HTTPDecoder coder where
  type DecodeSchema coder :: Type -> Type
  type DecodingError coder :: Type

  parseResponse ::
    coder ->
    DecodeSchema coder a ->
    HTTP.BodyReader ->
    IO (Either (DecodingError coder) a)

newtype HTTPResponseDecodingError
  = HTTPResponseDecodingError String
  deriving (Show)

instance Exc.Exception HTTPResponseDecodingError
