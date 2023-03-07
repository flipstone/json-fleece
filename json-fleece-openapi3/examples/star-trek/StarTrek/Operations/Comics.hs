{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Comics
  ( Comics(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Comics = Comics
  deriving (Eq, Show)

route :: R.Router r => r Comics
route =
  R.get $
    R.make Comics
      /- "comics"