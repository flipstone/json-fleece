{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementBase.HypersonicSeries
  ( HypersonicSeries(..)
  , hypersonicSeriesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HypersonicSeries = HypersonicSeries Bool
  deriving (Show, Eq)

hypersonicSeriesSchema :: FC.Fleece schema => schema HypersonicSeries
hypersonicSeriesSchema =
  FC.coerceSchema FC.boolean