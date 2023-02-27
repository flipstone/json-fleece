{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesFull.ProductionStartYear
  ( ProductionStartYear(..)
  , productionStartYearSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ProductionStartYear = ProductionStartYear Integer
  deriving (Show, Eq)

productionStartYearSchema :: FC.Fleece schema => schema ProductionStartYear
productionStartYearSchema =
  FC.coerceSchema FC.integer