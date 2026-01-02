{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.NetflixDigitalRelease
  ( NetflixDigitalRelease(..)
  , netflixDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype NetflixDigitalRelease = NetflixDigitalRelease Bool
  deriving (Show, Eq)

netflixDigitalReleaseSchema :: FC.Fleece t => FC.Schema t NetflixDigitalRelease
netflixDigitalReleaseSchema =
  FC.coerceSchema FC.boolean