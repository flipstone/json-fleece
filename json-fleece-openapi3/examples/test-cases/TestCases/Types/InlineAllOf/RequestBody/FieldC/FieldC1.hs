{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineAllOf.RequestBody.FieldC.FieldC1
  ( FieldC1(..)
  , fieldC1Schema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldC1 = FieldC1 T.Text
  deriving (Show, Eq)

fieldC1Schema :: FC.Fleece schema => schema FieldC1
fieldC1Schema =
  FC.coerceSchema FC.text