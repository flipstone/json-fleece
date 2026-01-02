{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.AtomicNumber
  ( AtomicNumber(..)
  , atomicNumberSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AtomicNumber = AtomicNumber Integer
  deriving (Show, Eq)

atomicNumberSchema :: FC.Fleece t => FC.Schema t AtomicNumber
atomicNumberSchema =
  FC.coerceSchema FC.integer