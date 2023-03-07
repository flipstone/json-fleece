{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Animal
  ( Animal(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Animal = Animal
  deriving (Eq, Show)

route :: R.Router r => r Animal
route =
  R.get $
    R.make Animal
      /- "animal"