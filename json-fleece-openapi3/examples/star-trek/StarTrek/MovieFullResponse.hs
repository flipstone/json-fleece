{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFullResponse
  ( MovieFullResponse(..)
  , movieFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MovieFull (MovieFull, movieFullSchema)

data MovieFullResponse = MovieFullResponse
  { movie :: Maybe MovieFull -- ^ Full movie, returned when queried using UID
  }
  deriving (Eq, Show)

movieFullResponseSchema :: FC.Fleece schema => schema MovieFullResponse
movieFullResponseSchema =
  FC.object $
    FC.constructor MovieFullResponse
      #+ FC.optional "movie" movie movieFullSchema