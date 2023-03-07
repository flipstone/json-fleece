{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Season
  ( Season(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Season = Season
  deriving (Eq, Show)

route :: R.Router r => r Season
route =
  R.get $
    R.make Season
      /- "season"