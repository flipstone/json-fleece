{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.Book
  ( Book(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data Book = Book
  deriving (Eq, Show)

route :: R.Router r => r Book
route =
  R.get $
    R.make Book
      /- "book"