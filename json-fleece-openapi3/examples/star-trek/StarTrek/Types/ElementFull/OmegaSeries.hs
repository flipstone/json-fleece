{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementFull.OmegaSeries
  ( OmegaSeries(..)
  , omegaSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype OmegaSeries = OmegaSeries Bool
  deriving (Show, Eq)

omegaSeriesSchema :: FC.Fleece t => FC.Schema t OmegaSeries
omegaSeriesSchema =
  FC.coerceSchema FC.boolean