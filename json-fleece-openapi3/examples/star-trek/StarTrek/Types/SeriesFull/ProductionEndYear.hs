{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesFull.ProductionEndYear
  ( ProductionEndYear(..)
  , productionEndYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ProductionEndYear = ProductionEndYear Integer
  deriving (Show, Eq)

productionEndYearSchema :: FC.Fleece schema => schema ProductionEndYear
productionEndYearSchema =
  FC.coerceSchema FC.integer