{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Operations.Habitat.Option1.Land
  ( Land(..)
  , landSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Land = Land T.Text
  deriving (Show, Eq)

landSchema :: FC.Fleece t => FC.Schema t Land
landSchema =
  FC.coerceSchema FC.text