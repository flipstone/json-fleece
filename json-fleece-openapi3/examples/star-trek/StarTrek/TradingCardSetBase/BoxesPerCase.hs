{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBase.BoxesPerCase
  ( BoxesPerCase(..)
  , boxesPerCaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype BoxesPerCase = BoxesPerCase Integer
  deriving (Show, Eq)

boxesPerCaseSchema :: FC.Fleece schema => schema BoxesPerCase
boxesPerCaseSchema =
  FC.coerceSchema FC.integer