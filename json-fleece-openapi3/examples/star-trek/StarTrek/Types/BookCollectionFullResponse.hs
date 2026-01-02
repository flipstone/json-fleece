{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionFullResponse
  ( BookCollectionFullResponse(..)
  , bookCollectionFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookCollectionFull as BookCollectionFull

data BookCollectionFullResponse = BookCollectionFullResponse
  { bookCollection :: Maybe BookCollectionFull.BookCollectionFull -- ^ Full book collection, returned when queried using UID
  }
  deriving (Eq, Show)

bookCollectionFullResponseSchema :: FC.Fleece t => FC.Schema t BookCollectionFullResponse
bookCollectionFullResponseSchema =
  FC.object $
    FC.constructor BookCollectionFullResponse
      #+ FC.optional "bookCollection" bookCollection BookCollectionFull.bookCollectionFullSchema