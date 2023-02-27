{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.NetflixDigitalRelease
  ( NetflixDigitalRelease(..)
  , netflixDigitalReleaseSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype NetflixDigitalRelease = NetflixDigitalRelease Bool
  deriving (Show, Eq)

netflixDigitalReleaseSchema :: FC.Fleece schema => schema NetflixDigitalRelease
netflixDigitalReleaseSchema =
  FC.coerceSchema FC.boolean