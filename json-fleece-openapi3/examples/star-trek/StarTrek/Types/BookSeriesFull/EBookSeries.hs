{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesFull.EBookSeries
  ( EBookSeries(..)
  , eBookSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EBookSeries = EBookSeries Bool
  deriving (Show, Eq)

eBookSeriesSchema :: FC.Fleece t => FC.Schema t EBookSeries
eBookSeriesSchema =
  FC.coerceSchema FC.boolean