{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.AtomicNumber
  ( AtomicNumber(..)
  , atomicNumberSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AtomicNumber = AtomicNumber Integer
  deriving (Show, Eq)

atomicNumberSchema :: FC.Fleece schema => schema AtomicNumber
atomicNumberSchema =
  FC.coerceSchema FC.integer