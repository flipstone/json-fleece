{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Weapon
  ( Weapon(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Weapon = Weapon
  deriving (Eq, Show)

route :: R.Router r => r Weapon
route =
  R.get $
    R.make Weapon
      /- "weapon"