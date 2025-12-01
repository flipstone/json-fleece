{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody.FieldB
  ( FieldB(..)
  , fieldBSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldB = FieldB T.Text
  deriving (Show, Eq)

fieldBSchema :: FC.Fleece schema => schema FieldB
fieldBSchema =
  FC.coerceSchema FC.text