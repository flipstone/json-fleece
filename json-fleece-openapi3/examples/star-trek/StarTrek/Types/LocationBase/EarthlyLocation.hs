{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.EarthlyLocation
  ( EarthlyLocation(..)
  , earthlyLocationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthlyLocation = EarthlyLocation Bool
  deriving (Show, Eq)

earthlyLocationSchema :: FC.Fleece schema => schema EarthlyLocation
earthlyLocationSchema =
  FC.coerceSchema FC.boolean