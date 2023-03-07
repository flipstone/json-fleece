{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Soundtrack
  ( Soundtrack(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Soundtrack = Soundtrack
  deriving (Eq, Show)

route :: R.Router r => r Soundtrack
route =
  R.get $
    R.make Soundtrack
      /- "soundtrack"