{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.ArrayFieldItem
  ( ArrayFieldItem(..)
  , arrayFieldItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ArrayFieldItem = ArrayFieldItem T.Text
  deriving (Show, Eq)

arrayFieldItemSchema :: FC.Fleece schema => schema ArrayFieldItem
arrayFieldItemSchema =
  FC.coerceSchema FC.text