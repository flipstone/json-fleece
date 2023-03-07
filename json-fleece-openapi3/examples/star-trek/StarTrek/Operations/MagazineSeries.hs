{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.MagazineSeries
  ( MagazineSeries(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data MagazineSeries = MagazineSeries
  deriving (Eq, Show)

route :: R.Router r => r MagazineSeries
route =
  R.get $
    R.make MagazineSeries
      /- "magazineSeries"