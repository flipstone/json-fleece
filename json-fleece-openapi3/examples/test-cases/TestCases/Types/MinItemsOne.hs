{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOne
  ( MinItemsOne(..)
  , minItemsOneSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.MinItemsOne.MinItemsOneItem as MinItemsOneItem

newtype MinItemsOne = MinItemsOne (NEL.NonEmpty MinItemsOneItem.MinItemsOneItem)
  deriving (Show, Eq)

minItemsOneSchema :: FC.Fleece t => FC.Schema t MinItemsOne
minItemsOneSchema =
  FC.coerceSchema (FC.nonEmpty MinItemsOneItem.minItemsOneItemSchema)