{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Episode.Search
  ( Search(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Search = Search
  deriving (Eq, Show)

route :: R.Router r => r Search
route =
  R.get $
    R.make Search
      /- "episode"
      /- "search"