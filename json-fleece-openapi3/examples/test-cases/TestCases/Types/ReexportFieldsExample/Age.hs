{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ReexportFieldsExample.Age
  ( Age(..)
  , ageSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Age = Age I.Int32
  deriving (Show, Eq)

ageSchema :: FC.Fleece t => FC.Schema t Age
ageSchema =
  FC.coerceSchema FC.int32