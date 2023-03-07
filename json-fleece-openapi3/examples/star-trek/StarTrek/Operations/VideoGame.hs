{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.VideoGame
  ( VideoGame(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data VideoGame = VideoGame
  deriving (Eq, Show)

route :: R.Router r => r VideoGame
route =
  R.get $
    R.make VideoGame
      /- "videoGame"