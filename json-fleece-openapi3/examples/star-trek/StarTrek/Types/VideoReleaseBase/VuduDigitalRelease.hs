{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.VuduDigitalRelease
  ( VuduDigitalRelease(..)
  , vuduDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VuduDigitalRelease = VuduDigitalRelease Bool
  deriving (Show, Eq)

vuduDigitalReleaseSchema :: FC.Fleece t => FC.Schema t VuduDigitalRelease
vuduDigitalReleaseSchema =
  FC.coerceSchema FC.boolean