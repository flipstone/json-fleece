{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFullResponse
  ( MovieFullResponse(..)
  , movieFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MovieFull as MovieFull

data MovieFullResponse = MovieFullResponse
  { movie :: Maybe MovieFull.MovieFull -- ^ Full movie, returned when queried using UID
  }
  deriving (Eq, Show)

movieFullResponseSchema :: FC.Fleece schema => schema MovieFullResponse
movieFullResponseSchema =
  FC.object $
    FC.constructor MovieFullResponse
      #+ FC.optional "movie" movie MovieFull.movieFullSchema