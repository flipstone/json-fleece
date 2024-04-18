{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinLengthOne
  ( MinLengthOne(..)
  , minLengthOneSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.MinLengthOne.MinLengthOneItem as MinLengthOneItem

newtype MinLengthOne = MinLengthOne (NEL.NonEmpty MinLengthOneItem.MinLengthOneItem)
  deriving (Show, Eq)

minLengthOneSchema :: FC.Fleece schema => schema MinLengthOne
minLengthOneSchema =
  FC.coerceSchema (FC.nonEmpty MinLengthOneItem.minLengthOneItemSchema)