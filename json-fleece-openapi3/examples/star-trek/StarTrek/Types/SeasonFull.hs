{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeasonFull
  ( SeasonFull(..)
  , seasonFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.EpisodeBase as EpisodeBase
import qualified StarTrek.Types.SeasonFull.NumberOfEpisodes as NumberOfEpisodes
import qualified StarTrek.Types.SeasonFull.SeasonNumber as SeasonNumber
import qualified StarTrek.Types.SeasonFull.Title as Title
import qualified StarTrek.Types.SeasonFull.Uid as Uid
import qualified StarTrek.Types.SeriesBase as SeriesBase

data SeasonFull = SeasonFull
  { title :: Title.Title -- ^ Season title
  , numberOfEpisodes :: Maybe NumberOfEpisodes.NumberOfEpisodes -- ^ Number of episodes in this season
  , uid :: Uid.Uid -- ^ Season unique ID
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number in series
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , series :: Maybe SeriesBase.SeriesBase -- ^ Base series, returned in search results
  }
  deriving (Eq, Show)

seasonFullSchema :: FC.Fleece schema => schema SeasonFull
seasonFullSchema =
  FC.object $
    FC.constructor SeasonFull
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes NumberOfEpisodes.numberOfEpisodesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "series" series SeriesBase.seriesBaseSchema