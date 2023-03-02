{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBaseResponse
  ( EpisodeBaseResponse(..)
  , episodeBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.EpisodeBase as EpisodeBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data EpisodeBaseResponse = EpisodeBaseResponse
  { episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

episodeBaseResponseSchema :: FC.Fleece schema => schema EpisodeBaseResponse
episodeBaseResponseSchema =
  FC.object $
    FC.constructor EpisodeBaseResponse
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema