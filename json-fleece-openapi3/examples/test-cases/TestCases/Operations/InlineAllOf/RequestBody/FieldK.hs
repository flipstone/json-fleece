{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody.FieldK
  ( FieldK(..)
  , fieldKSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldK = FieldK (Map.Map T.Text FC.AnyJSON)
  deriving (Show, Eq)

fieldKSchema :: FC.Fleece schema => schema FieldK
fieldKSchema =
  FC.coerceSchema (FC.map FC.anyJSON)