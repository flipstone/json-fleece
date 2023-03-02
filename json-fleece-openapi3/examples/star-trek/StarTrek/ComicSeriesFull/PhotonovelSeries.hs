{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesFull.PhotonovelSeries
  ( PhotonovelSeries(..)
  , photonovelSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PhotonovelSeries = PhotonovelSeries Bool
  deriving (Show, Eq)

photonovelSeriesSchema :: FC.Fleece schema => schema PhotonovelSeries
photonovelSeriesSchema =
  FC.coerceSchema FC.boolean