{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Occupation
  ( Occupation(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Occupation = Occupation
  deriving (Eq, Show)

route :: R.Router r => r Occupation
route =
  R.get $
    R.make Occupation
      /- "occupation"