{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.UltraVioletDigitalRelease
  ( UltraVioletDigitalRelease(..)
  , ultraVioletDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype UltraVioletDigitalRelease = UltraVioletDigitalRelease Bool
  deriving (Show, Eq)

ultraVioletDigitalReleaseSchema :: FC.Fleece t => FC.Schema t UltraVioletDigitalRelease
ultraVioletDigitalReleaseSchema =
  FC.coerceSchema FC.boolean