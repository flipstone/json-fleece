{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.GetInventory.Response200Body.Meta.GeneratedAt
  ( GeneratedAt(..)
  , generatedAtSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype GeneratedAt = GeneratedAt T.Text
  deriving (Show, Eq)

generatedAtSchema :: FC.Fleece t => FC.Schema t GeneratedAt
generatedAtSchema =
  FC.coerceSchema FC.text