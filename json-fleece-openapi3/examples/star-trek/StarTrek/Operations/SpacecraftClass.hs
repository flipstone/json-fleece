{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.SpacecraftClass
  ( SpacecraftClass(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data SpacecraftClass = SpacecraftClass
  deriving (Eq, Show)

route :: R.Router r => r SpacecraftClass
route =
  R.get $
    R.make SpacecraftClass
      /- "spacecraftClass"