{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.ITunesDigitalRelease
  ( ITunesDigitalRelease(..)
  , iTunesDigitalReleaseSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ITunesDigitalRelease = ITunesDigitalRelease Bool
  deriving (Show, Eq)

iTunesDigitalReleaseSchema :: FC.Fleece schema => schema ITunesDigitalRelease
iTunesDigitalReleaseSchema =
  FC.coerceSchema FC.boolean