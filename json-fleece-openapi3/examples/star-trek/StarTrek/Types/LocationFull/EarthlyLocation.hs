{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.EarthlyLocation
  ( EarthlyLocation(..)
  , earthlyLocationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthlyLocation = EarthlyLocation Bool
  deriving (Show, Eq)

earthlyLocationSchema :: FC.Fleece t => FC.Schema t EarthlyLocation
earthlyLocationSchema =
  FC.coerceSchema FC.boolean