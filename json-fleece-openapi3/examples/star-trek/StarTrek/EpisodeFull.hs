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
      #+ FC.optional "productionSerialNumber" productionSerialNumber FC.text
      #+ FC.optional "titleJapanese" titleJapanese FC.text
      #+ FC.optional "directors" directors (FC.list staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.number
      #+ FC.optional "episodeNumber" episodeNumber FC.integer
      #+ FC.optional "standInPerformers" standInPerformers (FC.list performerBaseSchema)
      #+ FC.optional "titleItalian" titleItalian FC.text
      #+ FC.optional "performers" performers (FC.list performerBaseSchema)
      #+ FC.optional "stuntPerformers" stuntPerformers (FC.list performerBaseSchema)
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "storyAuthors" storyAuthors (FC.list staffBaseSchema)
      #+ FC.optional "finalScriptDate" finalScriptDate FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.number
      #+ FC.optional "usAirDate" usAirDate FC.text
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "featureLength" featureLength FC.boolean
      #+ FC.optional "titleGerman" titleGerman FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "series" series seriesBaseSchema
      #+ FC.optional "season" season seasonBaseSchema
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "seasonNumber" seasonNumber FC.integer
      #+ FC.optional "teleplayAuthors" teleplayAuthors (FC.list staffBaseSchema)