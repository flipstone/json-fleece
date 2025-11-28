{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body.FieldM
  ( FieldM(..)
  , fieldMSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldM = FieldM T.Text
  deriving (Show, Eq)

fieldMSchema :: FC.Fleece schema => schema FieldM
fieldMSchema =
  FC.coerceSchema FC.text