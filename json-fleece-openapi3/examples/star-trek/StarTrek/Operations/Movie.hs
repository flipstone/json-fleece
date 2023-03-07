{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Movie
  ( Movie(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Movie = Movie
  deriving (Eq, Show)

route :: R.Router r => r Movie
route =
  R.get $
    R.make Movie
      /- "movie"