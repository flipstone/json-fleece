{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.BookCollection
  ( BookCollection(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data BookCollection = BookCollection
  deriving (Eq, Show)

route :: R.Router r => r BookCollection
route =
  R.get $
    R.make BookCollection
      /- "bookCollection"