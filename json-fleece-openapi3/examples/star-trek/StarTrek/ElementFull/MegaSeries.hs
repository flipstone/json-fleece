{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementFull.MegaSeries
  ( MegaSeries(..)
  , megaSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MegaSeries = MegaSeries Bool
  deriving (Show, Eq)

megaSeriesSchema :: FC.Fleece schema => schema MegaSeries
megaSeriesSchema =
  FC.coerceSchema FC.boolean