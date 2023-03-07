{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.WorldSeries
  ( WorldSeries(..)
  , worldSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype WorldSeries = WorldSeries Bool
  deriving (Show, Eq)

worldSeriesSchema :: FC.Fleece schema => schema WorldSeries
worldSeriesSchema =
  FC.coerceSchema FC.boolean