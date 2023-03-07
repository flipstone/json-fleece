{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Technology
  ( Technology(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Technology = Technology
  deriving (Eq, Show)

route :: R.Router r => r Technology
route =
  R.get $
    R.make Technology
      /- "technology"