{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementFull.TransonicSeries
  ( TransonicSeries(..)
  , transonicSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransonicSeries = TransonicSeries Bool
  deriving (Show, Eq)

transonicSeriesSchema :: FC.Fleece t => FC.Schema t TransonicSeries
transonicSeriesSchema =
  FC.coerceSchema FC.boolean