{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBase
  ( EpisodeBase(..)
  , episodeBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.SeasonHeader (SeasonHeader, seasonHeaderSchema)
import StarTrek.SeriesHeader (SeriesHeader, seriesHeaderSchema)

data EpisodeBase = EpisodeBase
  { productionSerialNumber :: Maybe Text -- ^ Production serial number
  , titleJapanese :: Maybe Text -- ^ Episode title in Japanese
  , yearFrom :: Maybe Integer -- ^ Starting year of episode story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of episode story
  , episodeNumber :: Maybe Integer -- ^ Episode number in season
  , titleItalian :: Maybe Text -- ^ Episode title in Italian
  , uid :: Text -- ^ Episode unique ID
  , finalScriptDate :: Maybe Text -- ^ Date the episode script was completed
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of episode story
  , usAirDate :: Maybe Text -- ^ Date the episode was first aired in the United States
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionSerialNumber" productionSerialNumber FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleJapanese" titleJapanese FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "episodeNumber" episodeNumber FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleItalian" titleItalian FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "finalScriptDate" finalScriptDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "usAirDate" usAirDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "featureLength" featureLength FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "series" series seriesHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "season" season seasonHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "seasonNumber" seasonNumber FC.integer