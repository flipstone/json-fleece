{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.OptionalField
  ( OptionalField(..)
  , optionalFieldSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalField = OptionalField T.Text
  deriving (Show, Eq)

optionalFieldSchema :: FC.Fleece t => FC.Schema t OptionalField
optionalFieldSchema =
  FC.coerceSchema FC.text