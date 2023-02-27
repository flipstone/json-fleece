{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBase
  ( EpisodeBase(..)
  , episodeBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.EpisodeBase.EpisodeNumber (EpisodeNumber, episodeNumberSchema)
import StarTrek.EpisodeBase.FeatureLength (FeatureLength, featureLengthSchema)
import StarTrek.EpisodeBase.FinalScriptDate (FinalScriptDate, finalScriptDateSchema)
import StarTrek.EpisodeBase.ProductionSerialNumber (ProductionSerialNumber, productionSerialNumberSchema)
import StarTrek.EpisodeBase.SeasonNumber (SeasonNumber, seasonNumberSchema)
import StarTrek.EpisodeBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.EpisodeBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.EpisodeBase.Title (Title, titleSchema)
import StarTrek.EpisodeBase.TitleGerman (TitleGerman, titleGermanSchema)
import StarTrek.EpisodeBase.TitleItalian (TitleItalian, titleItalianSchema)
import StarTrek.EpisodeBase.TitleJapanese (TitleJapanese, titleJapaneseSchema)
import StarTrek.EpisodeBase.Uid (Uid, uidSchema)
import StarTrek.EpisodeBase.UsAirDate (UsAirDate, usAirDateSchema)
import StarTrek.EpisodeBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.EpisodeBase.YearTo (YearTo, yearToSchema)
import StarTrek.SeasonHeader (SeasonHeader, seasonHeaderSchema)
import StarTrek.SeriesHeader (SeriesHeader, seriesHeaderSchema)

data EpisodeBase = EpisodeBase
  { productionSerialNumber :: Maybe ProductionSerialNumber -- ^ Production serial number
  , titleJapanese :: Maybe TitleJapanese -- ^ Episode title in Japanese
  , yearFrom :: Maybe YearFrom -- ^ Starting year of episode story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of episode story
  , episodeNumber :: Maybe EpisodeNumber -- ^ Episode number in season
  , titleItalian :: Maybe TitleItalian -- ^ Episode title in Italian
  , uid :: Uid -- ^ Episode unique ID
  , finalScriptDate :: Maybe FinalScriptDate -- ^ Date the episode script was completed
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of episode story
  , usAirDate :: Maybe UsAirDate -- ^ Date the episode was first aired in the United States
  , featureLength :: Maybe FeatureLength -- ^ Whether it's a feature length episode
  , titleGerman :: Maybe TitleGerman -- ^ Episode title in German
  , title :: Title -- ^ Episode title
  , yearTo :: Maybe YearTo -- ^ Ending year of episode story
  , series :: Maybe SeriesHeader -- ^ Header series, embedded in other objects
  , season :: Maybe SeasonHeader -- ^ Header season, embedded in other objects
  , seasonNumber :: Maybe SeasonNumber -- ^ Season number
  }
  deriving (Eq, Show)

episodeBaseSchema :: FC.Fleece schema => schema EpisodeBase
episodeBaseSchema =
  FC.object $
    FC.constructor EpisodeBase
      #+ FC.optional "productionSerialNumber" productionSerialNumber productionSerialNumberSchema
      #+ FC.optional "titleJapanese" titleJapanese titleJapaneseSchema
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "episodeNumber" episodeNumber episodeNumberSchema
      #+ FC.optional "titleItalian" titleItalian titleItalianSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "finalScriptDate" finalScriptDate finalScriptDateSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "usAirDate" usAirDate usAirDateSchema
      #+ FC.optional "featureLength" featureLength featureLengthSchema
      #+ FC.optional "titleGerman" titleGerman titleGermanSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "series" series seriesHeaderSchema
      #+ FC.optional "season" season seasonHeaderSchema
      #+ FC.optional "seasonNumber" seasonNumber seasonNumberSchema