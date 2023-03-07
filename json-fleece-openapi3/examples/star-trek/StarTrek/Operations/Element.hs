{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Element
  ( Element(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Element = Element
  deriving (Eq, Show)

route :: R.Router r => r Element
route =
  R.get $
    R.make Element
      /- "element"