{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonFull
  ( SeasonFull(..)
  , seasonFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.SeasonFull.NumberOfEpisodes (NumberOfEpisodes, numberOfEpisodesSchema)
import StarTrek.SeasonFull.SeasonNumber (SeasonNumber, seasonNumberSchema)
import StarTrek.SeasonFull.Title (Title, titleSchema)
import StarTrek.SeasonFull.Uid (Uid, uidSchema)
import StarTrek.SeriesBase (SeriesBase, seriesBaseSchema)

data SeasonFull = SeasonFull
  { uid :: Uid -- ^ Season unique ID
  , episodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , title :: Title -- ^ Season title
  , series :: Maybe SeriesBase -- ^ Base series, returned in search results
  , seasonNumber :: Maybe SeasonNumber -- ^ Season number in series
  , numberOfEpisodes :: Maybe NumberOfEpisodes -- ^ Number of episodes in this season
  }
  deriving (Eq, Show)

seasonFullSchema :: FC.Fleece schema => schema SeasonFull
seasonFullSchema =
  FC.object $
    FC.constructor SeasonFull
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.required "title" title titleSchema
      #+ FC.optional "series" series seriesBaseSchema
      #+ FC.optional "seasonNumber" seasonNumber seasonNumberSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes numberOfEpisodesSchema