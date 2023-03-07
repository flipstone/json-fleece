{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesBase.ProductionStartYear
  ( ProductionStartYear(..)
  , productionStartYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ProductionStartYear = ProductionStartYear Integer
  deriving (Show, Eq)

productionStartYearSchema :: FC.Fleece schema => schema ProductionStartYear
productionStartYearSchema =
  FC.coerceSchema FC.integer