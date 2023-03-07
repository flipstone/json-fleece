{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Staff
  ( Staff(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Staff = Staff
  deriving (Eq, Show)

route :: R.Router r => r Staff
route =
  R.get $
    R.make Staff
      /- "staff"