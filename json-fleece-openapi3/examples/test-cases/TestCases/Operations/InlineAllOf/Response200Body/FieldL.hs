{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body.FieldL
  ( FieldL(..)
  , fieldLSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldL = FieldL T.Text
  deriving (Show, Eq)

fieldLSchema :: FC.Fleece schema => schema FieldL
fieldLSchema =
  FC.coerceSchema FC.text