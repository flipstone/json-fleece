{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Activity.Uuid
  ( Uuid(..)
  , uuidSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Uuid = Uuid T.Text
  deriving (Show, Eq)

uuidSchema :: FC.Fleece t => FC.Schema t Uuid
uuidSchema =
  FC.coerceSchema FC.text