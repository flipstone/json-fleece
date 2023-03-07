{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Food
  ( Food(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Food = Food
  deriving (Eq, Show)

route :: R.Router r => r Food
route =
  R.get $
    R.make Food
      /- "food"