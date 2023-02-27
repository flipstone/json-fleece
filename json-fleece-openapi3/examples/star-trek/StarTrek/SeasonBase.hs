{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonBase
  ( SeasonBase(..)
  , seasonBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SeasonBase.NumberOfEpisodes (NumberOfEpisodes, numberOfEpisodesSchema)
import StarTrek.SeasonBase.SeasonNumber (SeasonNumber, seasonNumberSchema)
import StarTrek.SeasonBase.Title (Title, titleSchema)
import StarTrek.SeasonBase.Uid (Uid, uidSchema)
import StarTrek.SeriesHeader (SeriesHeader, seriesHeaderSchema)

data SeasonBase = SeasonBase
  { uid :: Uid -- ^ Season unique ID
  , title :: Title -- ^ Season title
  , series :: Maybe SeriesHeader -- ^ Header series, embedded in other objects
  , seasonNumber :: Maybe SeasonNumber -- ^ Season number in series
  , numberOfEpisodes :: Maybe NumberOfEpisodes -- ^ Number of episodes in this season
  }
  deriving (Eq, Show)

seasonBaseSchema :: FC.Fleece schema => schema SeasonBase
seasonBaseSchema =
  FC.object $
    FC.constructor SeasonBase
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "series" series seriesHeaderSchema
      #+ FC.optional "seasonNumber" seasonNumber seasonNumberSchema
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes numberOfEpisodesSchema