{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Series
  ( Series(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Series = Series
  deriving (Eq, Show)

route :: R.Router r => r Series
route =
  R.get $
    R.make Series
      /- "series"