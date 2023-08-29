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
  { storyAuthors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , title :: Title.Title -- ^ Episode title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of episode story
  , titleGerman :: Maybe TitleGerman.TitleGerman -- ^ Episode title in German
  , episodeNumber :: Maybe EpisodeNumber.EpisodeNumber -- ^ Episode number in season
  , productionSerialNumber :: Maybe ProductionSerialNumber.ProductionSerialNumber -- ^ Production serial number
  , featureLength :: Maybe FeatureLength.FeatureLength -- ^ Whether it's a feature length episode
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , titleItalian :: Maybe TitleItalian.TitleItalian -- ^ Episode title in Italian
  , season :: Maybe SeasonBase.SeasonBase -- ^ Base season, returned in search results
  , directors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , finalScriptDate :: Maybe FinalScriptDate.FinalScriptDate -- ^ Date the episode script was completed
  , usAirDate :: Maybe UsAirDate.UsAirDate -- ^ Date the episode was first aired in the United States
  , uid :: Uid.Uid -- ^ Episode unique ID
  , standInPerformers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of episode story
  , stuntPerformers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , teleplayAuthors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number
  , titleJapanese :: Maybe TitleJapanese.TitleJapanese -- ^ Episode title in Japanese
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of episode story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of episode story
  , series :: Maybe SeriesBase.SeriesBase -- ^ Base series, returned in search results
  , performers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  }
  deriving (Eq, Show)

episodeFullSchema :: FC.Fleece schema => schema EpisodeFull
episodeFullSchema =
  FC.object $
    FC.constructor EpisodeFull
      #+ FC.optional "storyAuthors" storyAuthors (FC.list StaffBase.staffBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "titleGerman" titleGerman TitleGerman.titleGermanSchema
      #+ FC.optional "episodeNumber" episodeNumber EpisodeNumber.episodeNumberSchema
      #+ FC.optional "productionSerialNumber" productionSerialNumber ProductionSerialNumber.productionSerialNumberSchema
      #+ FC.optional "featureLength" featureLength FeatureLength.featureLengthSchema
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "titleItalian" titleItalian TitleItalian.titleItalianSchema
      #+ FC.optional "season" season SeasonBase.seasonBaseSchema
      #+ FC.optional "directors" directors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "finalScriptDate" finalScriptDate FinalScriptDate.finalScriptDateSchema
      #+ FC.optional "usAirDate" usAirDate UsAirDate.usAirDateSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "standInPerformers" standInPerformers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "stuntPerformers" stuntPerformers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "teleplayAuthors" teleplayAuthors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema
      #+ FC.optional "titleJapanese" titleJapanese TitleJapanese.titleJapaneseSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "series" series SeriesBase.seriesBaseSchema
      #+ FC.optional "performers" performers (FC.list PerformerBase.performerBaseSchema)