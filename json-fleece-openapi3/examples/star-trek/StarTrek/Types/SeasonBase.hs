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
  { uid :: Uid.Uid -- ^ Season unique ID
  , title :: Title.Title -- ^ Season title
  , series :: Maybe SeriesHeader.SeriesHeader -- ^ Header series, embedded in other objects
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number in series
  , numberOfEpisodes :: Maybe NumberOfEpisodes.NumberOfEpisodes -- ^ Number of episodes in this season
  }
  deriving (Eq, Show)

seasonBaseSchema :: FC.Fleece schema => schema SeasonBase
seasonBaseSchema =
  FC.object $
    FC.constructor SeasonBase
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "series" series SeriesHeader.seriesHeaderSchema
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes NumberOfEpisodes.numberOfEpisodesSchema