{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBaseResponse
  ( EpisodeBaseResponse(..)
  , episodeBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data EpisodeBaseResponse = EpisodeBaseResponse
  { episodes :: Maybe [EpisodeBase] -- ^ List of episodes matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

episodeBaseResponseSchema :: FC.Fleece schema => schema EpisodeBaseResponse
episodeBaseResponseSchema =
  FC.object $
    FC.constructor EpisodeBaseResponse
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema