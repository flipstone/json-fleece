{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Magazine
  ( Magazine(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Magazine = Magazine
  deriving (Eq, Show)

route :: R.Router r => r Magazine
route =
  R.get $
    R.make Magazine
      /- "magazine"