{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.ArrayFieldItem
  ( ArrayFieldItem(..)
  , arrayFieldItemSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ArrayFieldItem = ArrayFieldItem Text
  deriving (Show, Eq)

arrayFieldItemSchema :: FC.Fleece schema => schema ArrayFieldItem
arrayFieldItemSchema =
  FC.coerceSchema FC.text