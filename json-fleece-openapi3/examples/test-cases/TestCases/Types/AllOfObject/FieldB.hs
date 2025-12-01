{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfObject.FieldB
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