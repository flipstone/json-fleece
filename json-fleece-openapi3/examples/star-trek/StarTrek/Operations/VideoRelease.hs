{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.VideoRelease
  ( VideoRelease(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data VideoRelease = VideoRelease
  deriving (Eq, Show)

route :: R.Router r => r VideoRelease
route =
  R.get $
    R.make VideoRelease
      /- "videoRelease"