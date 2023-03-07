{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Conflict
  ( Conflict(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Conflict = Conflict
  deriving (Eq, Show)

route :: R.Router r => r Conflict
route =
  R.get $
    R.make Conflict
      /- "conflict"