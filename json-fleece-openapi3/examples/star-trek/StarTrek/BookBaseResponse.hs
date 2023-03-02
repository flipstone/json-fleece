{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBaseResponse
  ( BookBaseResponse(..)
  , bookBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookBase as BookBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data BookBaseResponse = BookBaseResponse
  { books :: Maybe [BookBase.BookBase] -- ^ Base book, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

bookBaseResponseSchema :: FC.Fleece schema => schema BookBaseResponse
bookBaseResponseSchema =
  FC.object $
    FC.constructor BookBaseResponse
      #+ FC.optional "books" books (FC.list BookBase.bookBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema