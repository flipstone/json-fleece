{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Material
  ( Material(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Material = Material
  deriving (Eq, Show)

route :: R.Router r => r Material
route =
  R.get $
    R.make Material
      /- "material"