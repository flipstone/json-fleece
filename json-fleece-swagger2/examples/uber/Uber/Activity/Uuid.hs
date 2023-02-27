{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activity.Uuid
  ( Uuid(..)
  , uuidSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Uuid = Uuid Text
  deriving (Show, Eq)

uuidSchema :: FC.Fleece schema => schema Uuid
uuidSchema =
  FC.coerceSchema FC.text