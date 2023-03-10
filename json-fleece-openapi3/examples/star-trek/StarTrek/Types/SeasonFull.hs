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
  { uid :: Uid.Uid -- ^ Season unique ID
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , title :: Title.Title -- ^ Season title
  , series :: Maybe SeriesBase.SeriesBase -- ^ Base series, returned in search results
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number in series
  , numberOfEpisodes :: Maybe NumberOfEpisodes.NumberOfEpisodes -- ^ Number of episodes in this season
  }
  deriving (Eq, Show)

seasonFullSchema :: FC.Fleece schema => schema SeasonFull
seasonFullSchema =
  FC.object $
    FC.constructor SeasonFull
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "series" series SeriesBase.seriesBaseSchema
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes NumberOfEpisodes.numberOfEpisodesSchema