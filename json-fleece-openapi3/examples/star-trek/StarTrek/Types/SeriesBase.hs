{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesBase
  ( SeriesBase(..)
  , seriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyHeader as CompanyHeader
import qualified StarTrek.Types.SeriesBase.Abbreviation as Abbreviation
import qualified StarTrek.Types.SeriesBase.EpisodesCount as EpisodesCount
import qualified StarTrek.Types.SeriesBase.FeatureLengthEpisodesCount as FeatureLengthEpisodesCount
import qualified StarTrek.Types.SeriesBase.OriginalRunEndDate as OriginalRunEndDate
import qualified StarTrek.Types.SeriesBase.OriginalRunStartDate as OriginalRunStartDate
import qualified StarTrek.Types.SeriesBase.ProductionEndYear as ProductionEndYear
import qualified StarTrek.Types.SeriesBase.ProductionStartYear as ProductionStartYear
import qualified StarTrek.Types.SeriesBase.SeasonsCount as SeasonsCount
import qualified StarTrek.Types.SeriesBase.Title as Title
import qualified StarTrek.Types.SeriesBase.Uid as Uid

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