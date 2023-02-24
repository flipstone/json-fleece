{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionBaseResponse
  ( BookCollectionBaseResponse(..)
  , bookCollectionBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "bookCollections" bookCollections (FC.list bookCollectionBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema