{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Operations.Habitat.Option2.Water
  ( Water(..)
  , waterSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Water = Water T.Text
  deriving (Show, Eq)

waterSchema :: FC.Fleece t => FC.Schema t Water
waterSchema =
  FC.coerceSchema FC.text