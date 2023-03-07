{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Character
  ( Character(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Character = Character
  deriving (Eq, Show)

route :: R.Router r => r Character
route =
  R.get $
    R.make Character
      /- "character"