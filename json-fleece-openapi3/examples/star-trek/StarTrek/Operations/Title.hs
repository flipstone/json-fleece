{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Title
  ( Title(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Title = Title
  deriving (Eq, Show)

route :: R.Router r => r Title
route =
  R.get $
    R.make Title
      /- "title"