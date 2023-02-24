{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionFullResponse
  ( BookCollectionFullResponse(..)
  , bookCollectionFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookCollectionFull (BookCollectionFull, bookCollectionFullSchema)

data BookCollectionFullResponse = BookCollectionFullResponse
  { bookCollection :: Maybe BookCollectionFull -- ^ Full book collection, returned when queried using UID
  }
  deriving (Eq, Show)

bookCollectionFullResponseSchema :: FC.Fleece schema => schema BookCollectionFullResponse
bookCollectionFullResponseSchema =
  FC.object $
    FC.constructor BookCollectionFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "bookCollection" bookCollection bookCollectionFullSchema