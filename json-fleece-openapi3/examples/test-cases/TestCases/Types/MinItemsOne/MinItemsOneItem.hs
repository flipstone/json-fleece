{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOne.MinItemsOneItem
  ( MinItemsOneItem(..)
  , minItemsOneItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MinItemsOneItem = MinItemsOneItem Bool
  deriving (Show, Eq)

minItemsOneItemSchema :: FC.Fleece t => FC.Schema t MinItemsOneItem
minItemsOneItemSchema =
  FC.coerceSchema FC.boolean