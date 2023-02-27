{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.GooglePlayDigitalRelease
  ( GooglePlayDigitalRelease(..)
  , googlePlayDigitalReleaseSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GooglePlayDigitalRelease = GooglePlayDigitalRelease Bool
  deriving (Show, Eq)

googlePlayDigitalReleaseSchema :: FC.Fleece schema => schema GooglePlayDigitalRelease
googlePlayDigitalReleaseSchema =
  FC.coerceSchema FC.boolean