{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeBaseResponse
  ( EpisodeBaseResponse(..)
  , episodeBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.EpisodeBase as EpisodeBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data EpisodeBaseResponse = EpisodeBaseResponse
  { episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

episodeBaseResponseSchema :: FC.Fleece t => FC.Schema t EpisodeBaseResponse
episodeBaseResponseSchema =
  FC.object $
    FC.constructor EpisodeBaseResponse
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema