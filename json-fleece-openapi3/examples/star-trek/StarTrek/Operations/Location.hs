{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Location
  ( Location(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Location = Location
  deriving (Eq, Show)

route :: R.Router r => r Location
route =
  R.get $
    R.make Location
      /- "location"