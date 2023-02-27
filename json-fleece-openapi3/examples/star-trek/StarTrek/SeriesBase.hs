{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase
  ( SeriesBase(..)
  , seriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyHeader (CompanyHeader, companyHeaderSchema)
import StarTrek.SeriesBase.Abbreviation (Abbreviation, abbreviationSchema)
import StarTrek.SeriesBase.EpisodesCount (EpisodesCount, episodesCountSchema)
import StarTrek.SeriesBase.FeatureLengthEpisodesCount (FeatureLengthEpisodesCount, featureLengthEpisodesCountSchema)
import StarTrek.SeriesBase.OriginalRunEndDate (OriginalRunEndDate, originalRunEndDateSchema)
import StarTrek.SeriesBase.OriginalRunStartDate (OriginalRunStartDate, originalRunStartDateSchema)
import StarTrek.SeriesBase.ProductionEndYear (ProductionEndYear, productionEndYearSchema)
import StarTrek.SeriesBase.ProductionStartYear (ProductionStartYear, productionStartYearSchema)
import StarTrek.SeriesBase.SeasonsCount (SeasonsCount, seasonsCountSchema)
import StarTrek.SeriesBase.Title (Title, titleSchema)
import StarTrek.SeriesBase.Uid (Uid, uidSchema)

data SeriesBase = SeriesBase
  { productionCompany :: Maybe CompanyHeader -- ^ Header company, embedded in other objects
  , originalRunStartDate :: Maybe OriginalRunStartDate -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyHeader -- ^ Header company, embedded in other objects
  , productionStartYear :: Maybe ProductionStartYear -- ^ Year the series production started
  , uid :: Uid -- ^ Series unique ID
  , episodesCount :: Maybe EpisodesCount -- ^ Number of episodes
  , abbreviation :: Abbreviation -- ^ Series abbreviation
  , title :: Title -- ^ Series title
  , seasonsCount :: Maybe SeasonsCount -- ^ Number of seasons
  , featureLengthEpisodesCount :: Maybe FeatureLengthEpisodesCount -- ^ Number of feature length episodes
  , productionEndYear :: Maybe ProductionEndYear -- ^ Year the series production ended
  , originalRunEndDate :: Maybe OriginalRunEndDate -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesBaseSchema :: FC.Fleece schema => schema SeriesBase
seriesBaseSchema =
  FC.object $
    FC.constructor SeriesBase
      #+ FC.optional "productionCompany" productionCompany companyHeaderSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate originalRunStartDateSchema
      #+ FC.optional "originalBroadcaster" originalBroadcaster companyHeaderSchema
      #+ FC.optional "productionStartYear" productionStartYear productionStartYearSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "episodesCount" episodesCount episodesCountSchema
      #+ FC.required "abbreviation" abbreviation abbreviationSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "seasonsCount" seasonsCount seasonsCountSchema
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount featureLengthEpisodesCountSchema
      #+ FC.optional "productionEndYear" productionEndYear productionEndYearSchema
      #+ FC.optional "originalRunEndDate" originalRunEndDate originalRunEndDateSchema