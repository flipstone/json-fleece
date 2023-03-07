{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeFull
  ( EpisodeFull(..)
  , episodeFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.EpisodeFull.EpisodeNumber as EpisodeNumber
import qualified StarTrek.Types.EpisodeFull.FeatureLength as FeatureLength
import qualified StarTrek.Types.EpisodeFull.FinalScriptDate as FinalScriptDate
import qualified StarTrek.Types.EpisodeFull.ProductionSerialNumber as ProductionSerialNumber
import qualified StarTrek.Types.EpisodeFull.SeasonNumber as SeasonNumber
import qualified StarTrek.Types.EpisodeFull.StardateFrom as StardateFrom
import qualified StarTrek.Types.EpisodeFull.StardateTo as StardateTo
import qualified StarTrek.Types.EpisodeFull.Title as Title
import qualified StarTrek.Types.EpisodeFull.TitleGerman as TitleGerman
import qualified StarTrek.Types.EpisodeFull.TitleItalian as TitleItalian
import qualified StarTrek.Types.EpisodeFull.TitleJapanese as TitleJapanese
import qualified StarTrek.Types.EpisodeFull.Uid as Uid
import qualified StarTrek.Types.EpisodeFull.UsAirDate as UsAirDate
import qualified StarTrek.Types.EpisodeFull.YearFrom as YearFrom
import qualified StarTrek.Types.EpisodeFull.YearTo as YearTo
import qualified StarTrek.Types.PerformerBase as PerformerBase
import qualified StarTrek.Types.SeasonBase as SeasonBase
import qualified StarTrek.Types.SeriesBase as SeriesBase
import qualified StarTrek.Types.StaffBase as StaffBase

data EpisodeFull = EpisodeFull
  { productionSerialNumber :: Maybe ProductionSerialNumber.ProductionSerialNumber -- ^ Production serial number
  , titleJapanese :: Maybe TitleJapanese.TitleJapanese -- ^ Episode title in Japanese
  , directors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of episode story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of episode story
  , episodeNumber :: Maybe EpisodeNumber.EpisodeNumber -- ^ Episode number in season
  , standInPerformers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , titleItalian :: Maybe TitleItalian.TitleItalian -- ^ Episode title in Italian
  , performers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , stuntPerformers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , uid :: Uid.Uid -- ^ Episode unique ID
  , storyAuthors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , finalScriptDate :: Maybe FinalScriptDate.FinalScriptDate -- ^ Date the episode script was completed
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of episode story
  , usAirDate :: Maybe UsAirDate.UsAirDate -- ^ Date the episode was first aired in the United States
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , featureLength :: Maybe FeatureLength.FeatureLength -- ^ Whether it's a feature length episode
  , titleGerman :: Maybe TitleGerman.TitleGerman -- ^ Episode title in German
  , title :: Title.Title -- ^ Episode title
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of episode story
  , series :: Maybe SeriesBase.SeriesBase -- ^ Base series, returned in search results
  , season :: Maybe SeasonBase.SeasonBase -- ^ Base season, returned in search results
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number
  , teleplayAuthors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

episodeFullSchema :: FC.Fleece schema => schema EpisodeFull
episodeFullSchema =
  FC.object $
    FC.constructor EpisodeFull
      #+ FC.optional "productionSerialNumber" productionSerialNumber ProductionSerialNumber.productionSerialNumberSchema
      #+ FC.optional "titleJapanese" titleJapanese TitleJapanese.titleJapaneseSchema
      #+ FC.optional "directors" directors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "episodeNumber" episodeNumber EpisodeNumber.episodeNumberSchema
      #+ FC.optional "standInPerformers" standInPerformers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "titleItalian" titleItalian TitleItalian.titleItalianSchema
      #+ FC.optional "performers" performers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "stuntPerformers" stuntPerformers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "storyAuthors" storyAuthors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "finalScriptDate" finalScriptDate FinalScriptDate.finalScriptDateSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "usAirDate" usAirDate UsAirDate.usAirDateSchema
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "featureLength" featureLength FeatureLength.featureLengthSchema
      #+ FC.optional "titleGerman" titleGerman TitleGerman.titleGermanSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "series" series SeriesBase.seriesBaseSchema
      #+ FC.optional "season" season SeasonBase.seasonBaseSchema
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema
      #+ FC.optional "teleplayAuthors" teleplayAuthors (FC.list StaffBase.staffBaseSchema)