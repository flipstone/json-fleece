{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.ArrayFieldItem
  ( ArrayFieldItem(..)
  , arrayFieldItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ArrayFieldItem = ArrayFieldItem T.Text
  deriving (Show, Eq)

arrayFieldItemSchema :: FC.Fleece t => FC.Schema t ArrayFieldItem
arrayFieldItemSchema =
  FC.coerceSchema FC.text