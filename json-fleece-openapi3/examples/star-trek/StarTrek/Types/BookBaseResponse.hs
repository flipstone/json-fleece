{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBaseResponse
  ( BookBaseResponse(..)
  , bookBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookBase as BookBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data BookBaseResponse = BookBaseResponse
  { books :: Maybe [BookBase.BookBase] -- ^ Base book, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

bookBaseResponseSchema :: FC.Fleece schema => schema BookBaseResponse
bookBaseResponseSchema =
  FC.object $
    FC.constructor BookBaseResponse
      #+ FC.optional "books" books (FC.list BookBase.bookBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema