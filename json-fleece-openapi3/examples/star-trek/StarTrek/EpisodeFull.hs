{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeFull
  ( EpisodeFull(..)
  , episodeFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.EpisodeFull.EpisodeNumber (EpisodeNumber, episodeNumberSchema)
import StarTrek.EpisodeFull.FeatureLength (FeatureLength, featureLengthSchema)
import StarTrek.EpisodeFull.FinalScriptDate (FinalScriptDate, finalScriptDateSchema)
import StarTrek.EpisodeFull.ProductionSerialNumber (ProductionSerialNumber, productionSerialNumberSchema)
import StarTrek.EpisodeFull.SeasonNumber (SeasonNumber, seasonNumberSchema)
import StarTrek.EpisodeFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.EpisodeFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.EpisodeFull.Title (Title, titleSchema)
import StarTrek.EpisodeFull.TitleGerman (TitleGerman, titleGermanSchema)
import StarTrek.EpisodeFull.TitleItalian (TitleItalian, titleItalianSchema)
import StarTrek.EpisodeFull.TitleJapanese (TitleJapanese, titleJapaneseSchema)
import StarTrek.EpisodeFull.Uid (Uid, uidSchema)
import StarTrek.EpisodeFull.UsAirDate (UsAirDate, usAirDateSchema)
import StarTrek.EpisodeFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.EpisodeFull.YearTo (YearTo, yearToSchema)
import StarTrek.PerformerBase (PerformerBase, performerBaseSchema)
import StarTrek.SeasonBase (SeasonBase, seasonBaseSchema)
import StarTrek.SeriesBase (SeriesBase, seriesBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data EpisodeFull = EpisodeFull
  { productionSerialNumber :: Maybe ProductionSerialNumber -- ^ Production serial number
  , titleJapanese :: Maybe TitleJapanese -- ^ Episode title in Japanese
  , directors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom -- ^ Starting year of episode story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of episode story
  , episodeNumber :: Maybe EpisodeNumber -- ^ Episode number in season
  , standInPerformers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , titleItalian :: Maybe TitleItalian -- ^ Episode title in Italian
  , performers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , stuntPerformers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , uid :: Uid -- ^ Episode unique ID
  , storyAuthors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , finalScriptDate :: Maybe FinalScriptDate -- ^ Date the episode script was completed
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of episode story
  , usAirDate :: Maybe UsAirDate -- ^ Date the episode was first aired in the United States
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , featureLength :: Maybe FeatureLength -- ^ Whether it's a feature length episode
  , titleGerman :: Maybe TitleGerman -- ^ Episode title in German
  , title :: Title -- ^ Episode title
  , yearTo :: Maybe YearTo -- ^ Ending year of episode story
  , series :: Maybe SeriesBase -- ^ Base series, returned in search results
  , season :: Maybe SeasonBase -- ^ Base season, returned in search results
  , writers :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , seasonNumber :: Maybe SeasonNumber -- ^ Season number
  , teleplayAuthors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

episodeFullSchema :: FC.Fleece schema => schema EpisodeFull
episodeFullSchema =
  FC.object $
    FC.constructor EpisodeFull
      #+ FC.optional "productionSerialNumber" productionSerialNumber productionSerialNumberSchema
      #+ FC.optional "titleJapanese" titleJapanese titleJapaneseSchema
      #+ FC.optional "directors" directors (FC.list staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "episodeNumber" episodeNumber episodeNumberSchema
      #+ FC.optional "standInPerformers" standInPerformers (FC.list performerBaseSchema)
      #+ FC.optional "titleItalian" titleItalian titleItalianSchema
      #+ FC.optional "performers" performers (FC.list performerBaseSchema)
      #+ FC.optional "stuntPerformers" stuntPerformers (FC.list performerBaseSchema)
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "storyAuthors" storyAuthors (FC.list staffBaseSchema)
      #+ FC.optional "finalScriptDate" finalScriptDate finalScriptDateSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "usAirDate" usAirDate usAirDateSchema
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "featureLength" featureLength featureLengthSchema
      #+ FC.optional "titleGerman" titleGerman titleGermanSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "series" series seriesBaseSchema
      #+ FC.optional "season" season seasonBaseSchema
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "seasonNumber" seasonNumber seasonNumberSchema
      #+ FC.optional "teleplayAuthors" teleplayAuthors (FC.list staffBaseSchema)