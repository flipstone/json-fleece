{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.HypersonicSeries
  ( HypersonicSeries(..)
  , hypersonicSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HypersonicSeries = HypersonicSeries Bool
  deriving (Show, Eq)

hypersonicSeriesSchema :: FC.Fleece t => FC.Schema t HypersonicSeries
hypersonicSeriesSchema =
  FC.coerceSchema FC.boolean