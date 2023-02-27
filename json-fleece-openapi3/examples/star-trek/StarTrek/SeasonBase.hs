{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonBase
  ( SeasonBase(..)
  , seasonBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.SeriesHeader (SeriesHeader, seriesHeaderSchema)

data SeasonBase = SeasonBase
  { uid :: Text -- ^ Season unique ID
  , title :: Text -- ^ Season title
  , series :: Maybe SeriesHeader -- ^ Header series, embedded in other objects
  , seasonNumber :: Maybe Integer -- ^ Season number in series
  , numberOfEpisodes :: Maybe Integer -- ^ Number of episodes in this season
  }
  deriving (Eq, Show)

seasonBaseSchema :: FC.Fleece schema => schema SeasonBase
seasonBaseSchema =
  FC.object $
    FC.constructor SeasonBase
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "series" series seriesHeaderSchema
      #+ FC.optional "seasonNumber" seasonNumber FC.integer
      #+ FC.optional "numberOfEpisodes" numberOfEpisodes FC.integer