{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.XboxSmartGlassDigitalRelease
  ( XboxSmartGlassDigitalRelease(..)
  , xboxSmartGlassDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype XboxSmartGlassDigitalRelease = XboxSmartGlassDigitalRelease Bool
  deriving (Show, Eq)

xboxSmartGlassDigitalReleaseSchema :: FC.Fleece t => FC.Schema t XboxSmartGlassDigitalRelease
xboxSmartGlassDigitalReleaseSchema =
  FC.coerceSchema FC.boolean