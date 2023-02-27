{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFull.ProductionRun
  ( ProductionRun(..)
  , productionRunSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ProductionRun = ProductionRun Integer
  deriving (Show, Eq)

productionRunSchema :: FC.Fleece schema => schema ProductionRun
productionRunSchema =
  FC.coerceSchema FC.integer