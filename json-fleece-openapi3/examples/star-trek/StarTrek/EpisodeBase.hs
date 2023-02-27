{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBase
  ( EpisodeBase(..)
  , episodeBaseSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Float, Integer, Maybe, Show)
import StarTrek.SeasonHeader (SeasonHeader, seasonHeaderSchema)
import StarTrek.SeriesHeader (SeriesHeader, seriesHeaderSchema)

data EpisodeBase = EpisodeBase
  { productionSerialNumber :: Maybe Text -- ^ Production serial number
  , titleJapanese :: Maybe Text -- ^ Episode title in Japanese
  , yearFrom :: Maybe Integer -- ^ Starting year of episode story
  , stardateTo :: Maybe Float -- ^ Ending stardate of episode story
  , episodeNumber :: Maybe Integer -- ^ Episode number in season
  , titleItalian :: Maybe Text -- ^ Episode title in Italian
  , uid :: Text -- ^ Episode unique ID
  , finalScriptDate :: Maybe Day -- ^ Date the episode script was completed
  , stardateFrom :: Maybe Float -- ^ Starting stardate of episode story
  , usAirDate :: Maybe Day -- ^ Date the episode was first aired in the United States
  , featureLength :: Maybe Bool -- ^ Whether it's a feature length episode
  , titleGerman :: Maybe Text -- ^ Episode title in German
  , title :: Text -- ^ Episode title
  , yearTo :: Maybe Integer -- ^ Ending year of episode story
  , series :: Maybe SeriesHeader -- ^ Header series, embedded in other objects
  , season :: Maybe SeasonHeader -- ^ Header season, embedded in other objects
  , seasonNumber :: Maybe Integer -- ^ Season number
  }
  deriving (Eq, Show)

episodeBaseSchema :: FC.Fleece schema => schema EpisodeBase
episodeBaseSchema =
  FC.object $
    FC.constructor EpisodeBase
      #+ FC.optional "productionSerialNumber" productionSerialNumber FC.text
      #+ FC.optional "titleJapanese" titleJapanese FC.text
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
      #+ FC.optional "episodeNumber" episodeNumber FC.integer
      #+ FC.optional "titleItalian" titleItalian FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "finalScriptDate" finalScriptDate FC.day
      #+ FC.optional "stardateFrom" stardateFrom FC.float
      #+ FC.optional "usAirDate" usAirDate FC.day
      #+ FC.optional "featureLength" featureLength FC.boolean
      #+ FC.optional "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "series" series seriesHeaderSchema
      #+ FC.optional "season" season seasonHeaderSchema
      #+ FC.optional "seasonNumber" seasonNumber FC.integer