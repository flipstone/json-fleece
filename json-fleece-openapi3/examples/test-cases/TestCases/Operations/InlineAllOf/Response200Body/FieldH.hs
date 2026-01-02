{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body.FieldH
  ( FieldH(..)
  , fieldHSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldH = FieldH FC.Null
  deriving (Show, Eq)

fieldHSchema :: FC.Fleece t => FC.Schema t FieldH
fieldHSchema =
  FC.coerceSchema FC.null