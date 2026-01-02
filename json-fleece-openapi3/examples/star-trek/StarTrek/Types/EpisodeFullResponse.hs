{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeFullResponse
  ( EpisodeFullResponse(..)
  , episodeFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.EpisodeFull as EpisodeFull

data EpisodeFullResponse = EpisodeFullResponse
  { episode :: Maybe EpisodeFull.EpisodeFull -- ^ Full episode, returned when queried using UID
  }
  deriving (Eq, Show)

episodeFullResponseSchema :: FC.Fleece t => FC.Schema t EpisodeFullResponse
episodeFullResponseSchema =
  FC.object $
    FC.constructor EpisodeFullResponse
      #+ FC.optional "episode" episode EpisodeFull.episodeFullSchema