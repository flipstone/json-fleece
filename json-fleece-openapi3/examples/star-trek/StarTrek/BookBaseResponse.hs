{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBaseResponse
  ( BookBaseResponse(..)
  , bookBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookBase (BookBase, bookBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data BookBaseResponse = BookBaseResponse
  { books :: Maybe [BookBase] -- ^ List of books matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

bookBaseResponseSchema :: FC.Fleece schema => schema BookBaseResponse
bookBaseResponseSchema =
  FC.object $
    FC.constructor BookBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "books" books (FC.list bookBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema