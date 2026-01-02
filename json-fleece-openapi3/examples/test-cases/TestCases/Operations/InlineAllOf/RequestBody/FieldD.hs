{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody.FieldD
  ( FieldD(..)
  , fieldDSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype FieldD = FieldD Integer
  deriving (Show, Eq)

fieldDSchema :: FC.Fleece t => FC.Schema t FieldD
fieldDSchema =
  FC.coerceSchema FC.integer