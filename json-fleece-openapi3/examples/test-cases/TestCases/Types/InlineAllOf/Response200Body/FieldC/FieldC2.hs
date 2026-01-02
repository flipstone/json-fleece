{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineAllOf.Response200Body.FieldC.FieldC2
  ( FieldC2(..)
  , fieldC2Schema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldC2 = FieldC2 T.Text
  deriving (Show, Eq)

fieldC2Schema :: FC.Fleece t => FC.Schema t FieldC2
fieldC2Schema =
  FC.coerceSchema FC.text