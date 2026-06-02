{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Operations.GetInventory.Response200Body.Total
  ( Total(..)
  , totalSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Total = Total I.Int32
  deriving (Show, Eq)

totalSchema :: FC.Fleece t => FC.Schema t Total
totalSchema =
  FC.coerceSchema FC.int32