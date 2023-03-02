{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementBase.AtomicWeight
  ( AtomicWeight(..)
  , atomicWeightSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AtomicWeight = AtomicWeight Integer
  deriving (Show, Eq)

atomicWeightSchema :: FC.Fleece schema => schema AtomicWeight
atomicWeightSchema =
  FC.coerceSchema FC.integer