{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeasonBase
  ( SeasonBase(..)
  , seasonBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SeasonBase.NumberOfEpisodes as NumberOfEpisodes
import qualified StarTrek.Types.SeasonBase.SeasonNumber as SeasonNumber
import qualified StarTrek.Types.SeasonBase.Title as Title
import qualified StarTrek.Types.SeasonBase.Uid as Uid
import qualified StarTrek.Types.SeriesHeader as SeriesHeader

data SeasonBase = SeasonBase
  { numberOfEpisodes :: Maybe NumberOfEpisodes.NumberOfEpisodes -- ^ Number of episodes in this season
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number in series
  , series :: Maybe SeriesHeader.SeriesHeader -- ^ Header series, embedded in other objects
  , title :: Title.Title -- ^ Season title
  , uid :: Uid.Uid -- ^ Season unique ID
  }
  deriving (Eq, Show)

seasonBaseSchema :: FC.Fleece schema => schema SeasonBase
seasonBaseSchema =
  FC.object $
    FC.constructor SeasonBase
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes NumberOfEpisodes.numberOfEpisodesSchema
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema
      #+ FC.optional "series" series SeriesHeader.seriesHeaderSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema