{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody.FieldD
  ( FieldD(..)
  , fieldDSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype FieldD = FieldD Integer
  deriving (Show, Eq)

fieldDSchema :: FC.Fleece schema => schema FieldD
fieldDSchema =
  FC.coerceSchema FC.integer