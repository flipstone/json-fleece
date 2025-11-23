{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.RecursiveAllOf.Age
  ( Age(..)
  , ageSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Age = Age Integer
  deriving (Show, Eq)

ageSchema :: FC.Fleece schema => schema Age
ageSchema =
  FC.coerceSchema FC.integer