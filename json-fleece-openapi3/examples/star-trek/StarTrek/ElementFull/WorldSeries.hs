{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull.WorldSeries
  ( WorldSeries(..)
  , worldSeriesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype WorldSeries = WorldSeries Bool
  deriving (Show, Eq)

worldSeriesSchema :: FC.Fleece schema => schema WorldSeries
worldSeriesSchema =
  FC.coerceSchema FC.boolean