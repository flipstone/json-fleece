{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinLengthOne.MinLengthOneItem
  ( MinLengthOneItem(..)
  , minLengthOneItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MinLengthOneItem = MinLengthOneItem Bool
  deriving (Show, Eq)

minLengthOneItemSchema :: FC.Fleece schema => schema MinLengthOneItem
minLengthOneItemSchema =
  FC.coerceSchema FC.boolean