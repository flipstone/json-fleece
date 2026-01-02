{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementFull.MegaSeries
  ( MegaSeries(..)
  , megaSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MegaSeries = MegaSeries Bool
  deriving (Show, Eq)

megaSeriesSchema :: FC.Fleece t => FC.Schema t MegaSeries
megaSeriesSchema =
  FC.coerceSchema FC.boolean