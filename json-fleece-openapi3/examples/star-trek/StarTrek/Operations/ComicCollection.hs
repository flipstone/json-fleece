{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.ComicCollection
  ( ComicCollection(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data ComicCollection = ComicCollection
  deriving (Eq, Show)

route :: R.Router r => r ComicCollection
route =
  R.get $
    R.make ComicCollection
      /- "comicCollection"