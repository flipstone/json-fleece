{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.WorldSeries
  ( WorldSeries(..)
  , worldSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype WorldSeries = WorldSeries Bool
  deriving (Show, Eq)

worldSeriesSchema :: FC.Fleece t => FC.Schema t WorldSeries
worldSeriesSchema =
  FC.coerceSchema FC.boolean