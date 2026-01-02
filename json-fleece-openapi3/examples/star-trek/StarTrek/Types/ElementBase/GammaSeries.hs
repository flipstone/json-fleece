{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.GammaSeries
  ( GammaSeries(..)
  , gammaSeriesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GammaSeries = GammaSeries Bool
  deriving (Show, Eq)

gammaSeriesSchema :: FC.Fleece t => FC.Schema t GammaSeries
gammaSeriesSchema =
  FC.coerceSchema FC.boolean