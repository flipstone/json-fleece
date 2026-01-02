{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetFull.BoxesPerCase
  ( BoxesPerCase(..)
  , boxesPerCaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype BoxesPerCase = BoxesPerCase Integer
  deriving (Show, Eq)

boxesPerCaseSchema :: FC.Fleece t => FC.Schema t BoxesPerCase
boxesPerCaseSchema =
  FC.coerceSchema FC.integer