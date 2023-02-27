{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.OptionalArrayFieldItem
  ( OptionalArrayFieldItem(..)
  , optionalArrayFieldItemSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalArrayFieldItem = OptionalArrayFieldItem Text
  deriving (Show, Eq)

optionalArrayFieldItemSchema :: FC.Fleece schema => schema OptionalArrayFieldItem
optionalArrayFieldItemSchema =
  FC.coerceSchema FC.text