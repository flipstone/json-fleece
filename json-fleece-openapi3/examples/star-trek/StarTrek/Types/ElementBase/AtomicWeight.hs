{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.AtomicWeight
  ( AtomicWeight(..)
  , atomicWeightSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AtomicWeight = AtomicWeight Integer
  deriving (Show, Eq)

atomicWeightSchema :: FC.Fleece t => FC.Schema t AtomicWeight
atomicWeightSchema =
  FC.coerceSchema FC.integer