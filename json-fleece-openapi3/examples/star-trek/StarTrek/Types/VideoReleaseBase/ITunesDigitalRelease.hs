{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.ITunesDigitalRelease
  ( ITunesDigitalRelease(..)
  , iTunesDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ITunesDigitalRelease = ITunesDigitalRelease Bool
  deriving (Show, Eq)

iTunesDigitalReleaseSchema :: FC.Fleece schema => schema ITunesDigitalRelease
iTunesDigitalReleaseSchema =
  FC.coerceSchema FC.boolean