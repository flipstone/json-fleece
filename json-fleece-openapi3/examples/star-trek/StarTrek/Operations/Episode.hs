{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Episode
  ( Episode(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Episode = Episode
  deriving (Eq, Show)

route :: R.Router r => r Episode
route =
  R.get $
    R.make Episode
      /- "episode"