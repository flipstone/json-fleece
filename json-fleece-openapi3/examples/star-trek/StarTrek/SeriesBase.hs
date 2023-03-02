{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase
  ( SeriesBase(..)
  , seriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CompanyHeader as CompanyHeader
import qualified StarTrek.SeriesBase.Abbreviation as Abbreviation
import qualified StarTrek.SeriesBase.EpisodesCount as EpisodesCount
import qualified StarTrek.SeriesBase.FeatureLengthEpisodesCount as FeatureLengthEpisodesCount
import qualified StarTrek.SeriesBase.OriginalRunEndDate as OriginalRunEndDate
import qualified StarTrek.SeriesBase.OriginalRunStartDate as OriginalRunStartDate
import qualified StarTrek.SeriesBase.ProductionEndYear as ProductionEndYear
import qualified StarTrek.SeriesBase.ProductionStartYear as ProductionStartYear
import qualified StarTrek.SeriesBase.SeasonsCount as SeasonsCount
import qualified StarTrek.SeriesBase.Title as Title
import qualified StarTrek.SeriesBase.Uid as Uid

data SeriesBase = SeriesBase
  { productionCompany :: Maybe CompanyHeader.CompanyHeader -- ^ Header company, embedded in other objects
  , originalRunStartDate :: Maybe OriginalRunStartDate.OriginalRunStartDate -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyHeader.CompanyHeader -- ^ Header company, embedded in other objects
  , productionStartYear :: Maybe ProductionStartYear.ProductionStartYear -- ^ Year the series production started
  , uid :: Uid.Uid -- ^ Series unique ID
  , episodesCount :: Maybe EpisodesCount.EpisodesCount -- ^ Number of episodes
  , abbreviation :: Abbreviation.Abbreviation -- ^ Series abbreviation
  , title :: Title.Title -- ^ Series title
  , seasonsCount :: Maybe SeasonsCount.SeasonsCount -- ^ Number of seasons
  , featureLengthEpisodesCount :: Maybe FeatureLengthEpisodesCount.FeatureLengthEpisodesCount -- ^ Number of feature length episodes
  , productionEndYear :: Maybe ProductionEndYear.ProductionEndYear -- ^ Year the series production ended
  , originalRunEndDate :: Maybe OriginalRunEndDate.OriginalRunEndDate -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesBaseSchema :: FC.Fleece schema => schema SeriesBase
seriesBaseSchema =
  FC.object $
    FC.constructor SeriesBase
      #+ FC.optional "productionCompany" productionCompany CompanyHeader.companyHeaderSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate OriginalRunStartDate.originalRunStartDateSchema
      #+ FC.optional "originalBroadcaster" originalBroadcaster CompanyHeader.companyHeaderSchema
      #+ FC.optional "productionStartYear" productionStartYear ProductionStartYear.productionStartYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "episodesCount" episodesCount EpisodesCount.episodesCountSchema
      #+ FC.required "abbreviation" abbreviation Abbreviation.abbreviationSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "seasonsCount" seasonsCount SeasonsCount.seasonsCountSchema
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount FeatureLengthEpisodesCount.featureLengthEpisodesCountSchema
      #+ FC.optional "productionEndYear" productionEndYear ProductionEndYear.productionEndYearSchema
      #+ FC.optional "originalRunEndDate" originalRunEndDate OriginalRunEndDate.originalRunEndDateSchema