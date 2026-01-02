{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body.FieldN
  ( FieldN(..)
  , fieldNSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldN = FieldN T.Text
  deriving (Show, Eq)

fieldNSchema :: FC.Fleece t => FC.Schema t FieldN
fieldNSchema =
  FC.coerceSchema FC.text