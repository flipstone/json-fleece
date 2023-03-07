{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Literature
  ( Literature(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Literature = Literature
  deriving (Eq, Show)

route :: R.Router r => r Literature
route =
  R.get $
    R.make Literature
      /- "literature"