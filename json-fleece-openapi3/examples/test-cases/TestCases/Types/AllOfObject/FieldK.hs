{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfObject.FieldK
  ( FieldK(..)
  , fieldKSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldK = FieldK (Map.Map T.Text FC.AnyJSON)
  deriving (Show, Eq)

fieldKSchema :: FC.Fleece t => FC.Schema t FieldK
fieldKSchema =
  FC.coerceSchema (FC.map FC.anyJSON)