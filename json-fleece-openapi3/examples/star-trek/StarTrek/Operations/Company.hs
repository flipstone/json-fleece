{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Company
  ( Company(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Company = Company
  deriving (Eq, Show)

route :: R.Router r => r Company
route =
  R.get $
    R.make Company
      /- "company"