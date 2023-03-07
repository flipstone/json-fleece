{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.VuduDigitalRelease
  ( VuduDigitalRelease(..)
  , vuduDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VuduDigitalRelease = VuduDigitalRelease Bool
  deriving (Show, Eq)

vuduDigitalReleaseSchema :: FC.Fleece schema => schema VuduDigitalRelease
vuduDigitalReleaseSchema =
  FC.coerceSchema FC.boolean