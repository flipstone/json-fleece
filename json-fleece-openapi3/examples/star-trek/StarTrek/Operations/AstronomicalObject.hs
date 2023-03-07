{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.AstronomicalObject
  ( AstronomicalObject(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data AstronomicalObject = AstronomicalObject
  deriving (Eq, Show)

route :: R.Router r => r AstronomicalObject
route =
  R.get $
    R.make AstronomicalObject
      /- "astronomicalObject"