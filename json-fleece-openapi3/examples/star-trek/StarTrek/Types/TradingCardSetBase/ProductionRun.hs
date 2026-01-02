{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetBase.ProductionRun
  ( ProductionRun(..)
  , productionRunSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ProductionRun = ProductionRun Integer
  deriving (Show, Eq)

productionRunSchema :: FC.Fleece t => FC.Schema t ProductionRun
productionRunSchema =
  FC.coerceSchema FC.integer