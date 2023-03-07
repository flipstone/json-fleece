{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionBaseResponse
  ( BookCollectionBaseResponse(..)
  , bookCollectionBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookCollectionBase as BookCollectionBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data BookCollectionBaseResponse = BookCollectionBaseResponse
  { bookCollections :: Maybe [BookCollectionBase.BookCollectionBase] -- ^ Base book collection, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

bookCollectionBaseResponseSchema :: FC.Fleece schema => schema BookCollectionBaseResponse
bookCollectionBaseResponseSchema =
  FC.object $
    FC.constructor BookCollectionBaseResponse
      #+ FC.optional "bookCollections" bookCollections (FC.list BookCollectionBase.bookCollectionBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema