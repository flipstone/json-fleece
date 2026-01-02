{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.GooglePlayDigitalRelease
  ( GooglePlayDigitalRelease(..)
  , googlePlayDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GooglePlayDigitalRelease = GooglePlayDigitalRelease Bool
  deriving (Show, Eq)

googlePlayDigitalReleaseSchema :: FC.Fleece t => FC.Schema t GooglePlayDigitalRelease
googlePlayDigitalReleaseSchema =
  FC.coerceSchema FC.boolean