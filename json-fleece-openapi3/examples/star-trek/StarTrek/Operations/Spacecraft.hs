{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Spacecraft
  ( Spacecraft(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Spacecraft = Spacecraft
  deriving (Eq, Show)

route :: R.Router r => r Spacecraft
route =
  R.get $
    R.make Spacecraft
      /- "spacecraft"