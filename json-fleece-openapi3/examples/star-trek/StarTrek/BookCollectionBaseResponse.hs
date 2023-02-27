{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionBaseResponse
  ( BookCollectionBaseResponse(..)
  , bookCollectionBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookCollectionBase (BookCollectionBase, bookCollectionBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data BookCollectionBaseResponse = BookCollectionBaseResponse
  { bookCollections :: Maybe [BookCollectionBase] -- ^ List of book collections matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

bookCollectionBaseResponseSchema :: FC.Fleece schema => schema BookCollectionBaseResponse
bookCollectionBaseResponseSchema =
  FC.object $
    FC.constructor BookCollectionBaseResponse
      #+ FC.optional "bookCollections" bookCollections (FC.list bookCollectionBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema