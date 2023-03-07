{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Organization
  ( Organization(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Organization = Organization
  deriving (Eq, Show)

route :: R.Router r => r Organization
route =
  R.get $
    R.make Organization
      /- "organization"