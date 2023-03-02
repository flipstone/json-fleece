{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardBase.ProductionRun
  ( ProductionRun(..)
  , productionRunSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ProductionRun = ProductionRun Integer
  deriving (Show, Eq)

productionRunSchema :: FC.Fleece schema => schema ProductionRun
productionRunSchema =
  FC.coerceSchema FC.integer