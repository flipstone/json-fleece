{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody.FieldF
  ( FieldF(..)
  , fieldFSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldF = FieldF T.Text
  deriving (Show, Eq)

fieldFSchema :: FC.Fleece t => FC.Schema t FieldF
fieldFSchema =
  FC.coerceSchema FC.text