{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeFull
  ( EpisodeFull(..)
  , episodeFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.PerformerBase (PerformerBase, performerBaseSchema)
import StarTrek.SeasonBase (SeasonBase, seasonBaseSchema)
import StarTrek.SeriesBase (SeriesBase, seriesBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data EpisodeFull = EpisodeFull
  { productionSerialNumber :: Maybe Text -- ^ Production serial number
  , titleJapanese :: Maybe Text -- ^ Episode title in Japanese
  , directors :: Maybe [StaffBase] -- ^ Directors authors involved in the episode
  , yearFrom :: Maybe Integer -- ^ Starting year of episode story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of episode story
  , episodeNumber :: Maybe Integer -- ^ Episode number in season
  , standInPerformers :: Maybe [PerformerBase] -- ^ Stand-in performers appearing in the episode
  , titleItalian :: Maybe Text -- ^ Episode title in Italian
  , performers :: Maybe [PerformerBase] -- ^ Performers appearing in the episode
  , stuntPerformers :: Maybe [PerformerBase] -- ^ Stunt performers appearing in the episode
  , uid :: Text -- ^ Episode unique ID
  , storyAuthors :: Maybe [StaffBase] -- ^ Story authors involved in the episode
  , finalScriptDate :: Maybe Text -- ^ Date the episode script was completed
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of episode story
  , usAirDate :: Maybe Text -- ^ Date the episode was first aired in the United States
  , characters :: Maybe [CharacterBase] -- ^ Characters appearing in the episode
  , featureLength :: Maybe Bool -- ^ Whether it's a feature length episode
  , titleGerman :: Maybe Text -- ^ Episode title in German
  , title :: Text -- ^ Episode title
  , yearTo :: Maybe Integer -- ^ Ending year of episode story
  , series :: Maybe SeriesBase -- ^ Base series, returned in search results
  , season :: Maybe SeasonBase -- ^ Base season, returned in search results
  , writers :: Maybe [StaffBase] -- ^ Writers involved in the episode
  , seasonNumber :: Maybe Integer -- ^ Season number
  , teleplayAuthors :: Maybe [StaffBase] -- ^ Teleplay authors involved in the episode
  }
  deriving (Eq, Show)

episodeFullSchema :: FC.Fleece schema => schema EpisodeFull
episodeFullSchema =
  FC.object $
    FC.constructor EpisodeFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionSerialNumber" productionSerialNumber FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleJapanese" titleJapanese FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "directors" directors (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "episodeNumber" episodeNumber FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "standInPerformers" standInPerformers (FC.list performerBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleItalian" titleItalian FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "performers" performers (FC.list performerBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "stuntPerformers" stuntPerformers (FC.list performerBaseSchema)
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "storyAuthors" storyAuthors (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "finalScriptDate" finalScriptDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "usAirDate" usAirDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "featureLength" featureLength FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "series" series seriesBaseSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "season" season seasonBaseSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "writers" writers (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "seasonNumber" seasonNumber FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "teleplayAuthors" teleplayAuthors (FC.list staffBaseSchema)