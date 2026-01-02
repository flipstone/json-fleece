{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesBase.ProductionEndYear
  ( ProductionEndYear(..)
  , productionEndYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ProductionEndYear = ProductionEndYear Integer
  deriving (Show, Eq)

productionEndYearSchema :: FC.Fleece t => FC.Schema t ProductionEndYear
productionEndYearSchema =
  FC.coerceSchema FC.integer