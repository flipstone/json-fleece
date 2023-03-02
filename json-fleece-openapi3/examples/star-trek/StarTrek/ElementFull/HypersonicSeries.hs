{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull.HypersonicSeries
  ( HypersonicSeries(..)
  , hypersonicSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HypersonicSeries = HypersonicSeries Bool
  deriving (Show, Eq)

hypersonicSeriesSchema :: FC.Fleece schema => schema HypersonicSeries
hypersonicSeriesSchema =
  FC.coerceSchema FC.boolean