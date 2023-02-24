{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBaseResponse
  ( MovieBaseResponse(..)
  , movieBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MovieBase (MovieBase, movieBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MovieBaseResponse = MovieBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , movies :: Maybe [MovieBase] -- ^ List of movies matching given criteria
  }
  deriving (Eq, Show)

movieBaseResponseSchema :: FC.Fleece schema => schema MovieBaseResponse
movieBaseResponseSchema =
  FC.object $
    FC.constructor MovieBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "movies" movies (FC.list movieBaseSchema)