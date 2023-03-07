{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.BookSeries
  ( BookSeries(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data BookSeries = BookSeries
  deriving (Eq, Show)

route :: R.Router r => r BookSeries
route =
  R.get $
    R.make BookSeries
      /- "bookSeries"