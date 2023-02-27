{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.OptionalField
  ( OptionalField(..)
  , optionalFieldSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalField = OptionalField Text
  deriving (Show, Eq)

optionalFieldSchema :: FC.Fleece schema => schema OptionalField
optionalFieldSchema =
  FC.coerceSchema FC.text