{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.ComicStrip
  ( ComicStrip(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data ComicStrip = ComicStrip
  deriving (Eq, Show)

route :: R.Router r => r ComicStrip
route =
  R.get $
    R.make ComicStrip
      /- "comicStrip"