{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activity.Uuid
  ( Uuid(..)
  , uuidSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Uuid = Uuid T.Text
  deriving (Show, Eq)

uuidSchema :: FC.Fleece schema => schema Uuid
uuidSchema =
  FC.coerceSchema FC.text