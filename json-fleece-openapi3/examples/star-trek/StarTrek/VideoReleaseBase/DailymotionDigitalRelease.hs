{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.DailymotionDigitalRelease
  ( DailymotionDigitalRelease(..)
  , dailymotionDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DailymotionDigitalRelease = DailymotionDigitalRelease Bool
  deriving (Show, Eq)

dailymotionDigitalReleaseSchema :: FC.Fleece schema => schema DailymotionDigitalRelease
dailymotionDigitalReleaseSchema =
  FC.coerceSchema FC.boolean