{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Species
  ( Species(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Species = Species
  deriving (Eq, Show)

route :: R.Router r => r Species
route =
  R.get $
    R.make Species
      /- "species"