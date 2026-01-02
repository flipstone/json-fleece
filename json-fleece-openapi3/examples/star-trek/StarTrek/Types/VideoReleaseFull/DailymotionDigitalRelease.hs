{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.DailymotionDigitalRelease
  ( DailymotionDigitalRelease(..)
  , dailymotionDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DailymotionDigitalRelease = DailymotionDigitalRelease Bool
  deriving (Show, Eq)

dailymotionDigitalReleaseSchema :: FC.Fleece t => FC.Schema t DailymotionDigitalRelease
dailymotionDigitalReleaseSchema =
  FC.coerceSchema FC.boolean