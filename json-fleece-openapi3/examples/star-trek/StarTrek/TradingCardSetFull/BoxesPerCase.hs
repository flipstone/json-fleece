{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFull.BoxesPerCase
  ( BoxesPerCase(..)
  , boxesPerCaseSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype BoxesPerCase = BoxesPerCase Integer
  deriving (Show, Eq)

boxesPerCaseSchema :: FC.Fleece schema => schema BoxesPerCase
boxesPerCaseSchema =
  FC.coerceSchema FC.integer