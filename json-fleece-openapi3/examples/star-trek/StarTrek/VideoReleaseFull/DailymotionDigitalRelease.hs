{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.DailymotionDigitalRelease
  ( DailymotionDigitalRelease(..)
  , dailymotionDigitalReleaseSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DailymotionDigitalRelease = DailymotionDigitalRelease Bool
  deriving (Show, Eq)

dailymotionDigitalReleaseSchema :: FC.Fleece schema => schema DailymotionDigitalRelease
dailymotionDigitalReleaseSchema =
  FC.coerceSchema FC.boolean