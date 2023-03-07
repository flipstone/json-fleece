{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.OptionalArrayFieldItem
  ( OptionalArrayFieldItem(..)
  , optionalArrayFieldItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalArrayFieldItem = OptionalArrayFieldItem T.Text
  deriving (Show, Eq)

optionalArrayFieldItemSchema :: FC.Fleece schema => schema OptionalArrayFieldItem
optionalArrayFieldItemSchema =
  FC.coerceSchema FC.text