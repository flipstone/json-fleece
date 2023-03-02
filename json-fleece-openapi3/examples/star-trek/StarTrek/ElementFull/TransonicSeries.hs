{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull.TransonicSeries
  ( TransonicSeries(..)
  , transonicSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransonicSeries = TransonicSeries Bool
  deriving (Show, Eq)

transonicSeriesSchema :: FC.Fleece schema => schema TransonicSeries
transonicSeriesSchema =
  FC.coerceSchema FC.boolean