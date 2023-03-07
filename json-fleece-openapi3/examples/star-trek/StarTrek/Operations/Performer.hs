{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Performer
  ( Performer(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Performer = Performer
  deriving (Eq, Show)

route :: R.Router r => r Performer
route =
  R.get $
    R.make Performer
      /- "performer"