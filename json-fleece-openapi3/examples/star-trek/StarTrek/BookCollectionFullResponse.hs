{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionFullResponse
  ( BookCollectionFullResponse(..)
  , bookCollectionFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookCollectionFull as BookCollectionFull

data BookCollectionFullResponse = BookCollectionFullResponse
  { bookCollection :: Maybe BookCollectionFull.BookCollectionFull -- ^ Full book collection, returned when queried using UID
  }
  deriving (Eq, Show)

bookCollectionFullResponseSchema :: FC.Fleece schema => schema BookCollectionFullResponse
bookCollectionFullResponseSchema =
  FC.object $
    FC.constructor BookCollectionFullResponse
      #+ FC.optional "bookCollection" bookCollection BookCollectionFull.bookCollectionFullSchema