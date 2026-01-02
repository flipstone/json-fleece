{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFullResponse
  ( BookFullResponse(..)
  , bookFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookFull as BookFull

data BookFullResponse = BookFullResponse
  { book :: Maybe BookFull.BookFull -- ^ Full book, returned when queried using UID
  }
  deriving (Eq, Show)

bookFullResponseSchema :: FC.Fleece t => FC.Schema t BookFullResponse
bookFullResponseSchema =
  FC.object $
    FC.constructor BookFullResponse
      #+ FC.optional "book" book BookFull.bookFullSchema