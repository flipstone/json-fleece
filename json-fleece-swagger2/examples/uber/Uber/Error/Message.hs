{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Error.Message
  ( Message(..)
  , messageSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Message = Message T.Text
  deriving (Show, Eq)

messageSchema :: FC.Fleece schema => schema Message
messageSchema =
  FC.coerceSchema FC.text