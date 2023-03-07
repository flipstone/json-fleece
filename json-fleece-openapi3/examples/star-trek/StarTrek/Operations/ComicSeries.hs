{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.ComicSeries
  ( ComicSeries(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data ComicSeries = ComicSeries
  deriving (Eq, Show)

route :: R.Router r => r ComicSeries
route =
  R.get $
    R.make ComicSeries
      /- "comicSeries"