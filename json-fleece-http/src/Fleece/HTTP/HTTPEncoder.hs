{-# LANGUAGE TypeFamilies #-}

module Fleece.HTTP.HTTPEncoder
  ( HTTPEncoder (EncodeSchema, toRequestBody)
  ) where

import Data.Kind (Type)
import qualified Network.HTTP.Client as HTTP

class HTTPEncoder coder where
  type EncodeSchema coder :: Type -> Type
  toRequestBody :: coder -> EncodeSchema coder a -> a -> HTTP.RequestBody
