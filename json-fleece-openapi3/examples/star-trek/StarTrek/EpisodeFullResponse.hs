{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeFullResponse
  ( EpisodeFullResponse(..)
  , episodeFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.EpisodeFull (EpisodeFull, episodeFullSchema)

data EpisodeFullResponse = EpisodeFullResponse
  { episode :: Maybe EpisodeFull -- ^ Full episode, returned when queried using UID
  }
  deriving (Eq, Show)

episodeFullResponseSchema :: FC.Fleece schema => schema EpisodeFullResponse
episodeFullResponseSchema =
  FC.object $
    FC.constructor EpisodeFullResponse
      #+ FC.optional "episode" episode episodeFullSchema