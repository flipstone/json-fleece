{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody.FieldN
  ( FieldN(..)
  , fieldNSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldN = FieldN T.Text
  deriving (Show, Eq)

fieldNSchema :: FC.Fleece schema => schema FieldN
fieldNSchema =
  FC.coerceSchema FC.text