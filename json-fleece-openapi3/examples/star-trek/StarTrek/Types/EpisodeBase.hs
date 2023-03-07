{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeBase
  ( EpisodeBase(..)
  , episodeBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.EpisodeBase.EpisodeNumber as EpisodeNumber
import qualified StarTrek.Types.EpisodeBase.FeatureLength as FeatureLength
import qualified StarTrek.Types.EpisodeBase.FinalScriptDate as FinalScriptDate
import qualified StarTrek.Types.EpisodeBase.ProductionSerialNumber as ProductionSerialNumber
import qualified StarTrek.Types.EpisodeBase.SeasonNumber as SeasonNumber
import qualified StarTrek.Types.EpisodeBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.EpisodeBase.StardateTo as StardateTo
import qualified StarTrek.Types.EpisodeBase.Title as Title
import qualified StarTrek.Types.EpisodeBase.TitleGerman as TitleGerman
import qualified StarTrek.Types.EpisodeBase.TitleItalian as TitleItalian
import qualified StarTrek.Types.EpisodeBase.TitleJapanese as TitleJapanese
import qualified StarTrek.Types.EpisodeBase.Uid as Uid
import qualified StarTrek.Types.EpisodeBase.UsAirDate as UsAirDate
import qualified StarTrek.Types.EpisodeBase.YearFrom as YearFrom
import qualified StarTrek.Types.EpisodeBase.YearTo as YearTo
import qualified StarTrek.Types.SeasonHeader as SeasonHeader
import qualified StarTrek.Types.SeriesHeader as SeriesHeader

data EpisodeBase = EpisodeBase
  { productionSerialNumber :: Maybe ProductionSerialNumber.ProductionSerialNumber -- ^ Production serial number
  , titleJapanese :: Maybe TitleJapanese.TitleJapanese -- ^ Episode title in Japanese
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of episode story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of episode story
  , episodeNumber :: Maybe EpisodeNumber.EpisodeNumber -- ^ Episode number in season
  , titleItalian :: Maybe TitleItalian.TitleItalian -- ^ Episode title in Italian
  , uid :: Uid.Uid -- ^ Episode unique ID
  , finalScriptDate :: Maybe FinalScriptDate.FinalScriptDate -- ^ Date the episode script was completed
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of episode story
  , usAirDate :: Maybe UsAirDate.UsAirDate -- ^ Date the episode was first aired in the United States
  , featureLength :: Maybe FeatureLength.FeatureLength -- ^ Whether it's a feature length episode
  , titleGerman :: Maybe TitleGerman.TitleGerman -- ^ Episode title in German
  , title :: Title.Title -- ^ Episode title
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of episode story
  , series :: Maybe SeriesHeader.SeriesHeader -- ^ Header series, embedded in other objects
  , season :: Maybe SeasonHeader.SeasonHeader -- ^ Header season, embedded in other objects
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number
  }
  deriving (Eq, Show)

episodeBaseSchema :: FC.Fleece schema => schema EpisodeBase
episodeBaseSchema =
  FC.object $
    FC.constructor EpisodeBase
      #+ FC.optional "productionSerialNumber" productionSerialNumber ProductionSerialNumber.productionSerialNumberSchema
      #+ FC.optional "titleJapanese" titleJapanese TitleJapanese.titleJapaneseSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "episodeNumber" episodeNumber EpisodeNumber.episodeNumberSchema
      #+ FC.optional "titleItalian" titleItalian TitleItalian.titleItalianSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "finalScriptDate" finalScriptDate FinalScriptDate.finalScriptDateSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "usAirDate" usAirDate UsAirDate.usAirDateSchema
      #+ FC.optional "featureLength" featureLength FeatureLength.featureLengthSchema
      #+ FC.optional "titleGerman" titleGerman TitleGerman.titleGermanSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "series" series SeriesHeader.seriesHeaderSchema
      #+ FC.optional "season" season SeasonHeader.seasonHeaderSchema
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema