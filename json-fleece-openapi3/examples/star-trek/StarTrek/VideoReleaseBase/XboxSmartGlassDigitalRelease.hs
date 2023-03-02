{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.XboxSmartGlassDigitalRelease
  ( XboxSmartGlassDigitalRelease(..)
  , xboxSmartGlassDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype XboxSmartGlassDigitalRelease = XboxSmartGlassDigitalRelease Bool
  deriving (Show, Eq)

xboxSmartGlassDigitalReleaseSchema :: FC.Fleece schema => schema XboxSmartGlassDigitalRelease
xboxSmartGlassDigitalReleaseSchema =
  FC.coerceSchema FC.boolean