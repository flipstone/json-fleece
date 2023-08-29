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
  { title :: Title.Title -- ^ Episode title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of episode story
  , titleGerman :: Maybe TitleGerman.TitleGerman -- ^ Episode title in German
  , episodeNumber :: Maybe EpisodeNumber.EpisodeNumber -- ^ Episode number in season
  , productionSerialNumber :: Maybe ProductionSerialNumber.ProductionSerialNumber -- ^ Production serial number
  , featureLength :: Maybe FeatureLength.FeatureLength -- ^ Whether it's a feature length episode
  , titleItalian :: Maybe TitleItalian.TitleItalian -- ^ Episode title in Italian
  , season :: Maybe SeasonHeader.SeasonHeader -- ^ Header season, embedded in other objects
  , finalScriptDate :: Maybe FinalScriptDate.FinalScriptDate -- ^ Date the episode script was completed
  , usAirDate :: Maybe UsAirDate.UsAirDate -- ^ Date the episode was first aired in the United States
  , uid :: Uid.Uid -- ^ Episode unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of episode story
  , seasonNumber :: Maybe SeasonNumber.SeasonNumber -- ^ Season number
  , titleJapanese :: Maybe TitleJapanese.TitleJapanese -- ^ Episode title in Japanese
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of episode story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of episode story
  , series :: Maybe SeriesHeader.SeriesHeader -- ^ Header series, embedded in other objects
  }
  deriving (Eq, Show)

episodeBaseSchema :: FC.Fleece schema => schema EpisodeBase
episodeBaseSchema =
  FC.object $
    FC.constructor EpisodeBase
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "titleGerman" titleGerman TitleGerman.titleGermanSchema
      #+ FC.optional "episodeNumber" episodeNumber EpisodeNumber.episodeNumberSchema
      #+ FC.optional "productionSerialNumber" productionSerialNumber ProductionSerialNumber.productionSerialNumberSchema
      #+ FC.optional "featureLength" featureLength FeatureLength.featureLengthSchema
      #+ FC.optional "titleItalian" titleItalian TitleItalian.titleItalianSchema
      #+ FC.optional "season" season SeasonHeader.seasonHeaderSchema
      #+ FC.optional "finalScriptDate" finalScriptDate FinalScriptDate.finalScriptDateSchema
      #+ FC.optional "usAirDate" usAirDate UsAirDate.usAirDateSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "seasonNumber" seasonNumber SeasonNumber.seasonNumberSchema
      #+ FC.optional "titleJapanese" titleJapanese TitleJapanese.titleJapaneseSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "series" series SeriesHeader.seriesHeaderSchema