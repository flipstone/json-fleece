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
  { title :: Title.Title -- ^ Series title
  , productionStartYear :: Maybe ProductionStartYear.ProductionStartYear -- ^ Year the series production started
  , episodesCount :: Maybe EpisodesCount.EpisodesCount -- ^ Number of episodes
  , productionCompany :: Maybe CompanyHeader.CompanyHeader -- ^ Header company, embedded in other objects
  , seasonsCount :: Maybe SeasonsCount.SeasonsCount -- ^ Number of seasons
  , abbreviation :: Abbreviation.Abbreviation -- ^ Series abbreviation
  , uid :: Uid.Uid -- ^ Series unique ID
  , originalRunStartDate :: Maybe OriginalRunStartDate.OriginalRunStartDate -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyHeader.CompanyHeader -- ^ Header company, embedded in other objects
  , featureLengthEpisodesCount :: Maybe FeatureLengthEpisodesCount.FeatureLengthEpisodesCount -- ^ Number of feature length episodes
  , productionEndYear :: Maybe ProductionEndYear.ProductionEndYear -- ^ Year the series production ended
  , originalRunEndDate :: Maybe OriginalRunEndDate.OriginalRunEndDate -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesBaseSchema :: FC.Fleece schema => schema SeriesBase
seriesBaseSchema =
  FC.object $
    FC.constructor SeriesBase
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "productionStartYear" productionStartYear ProductionStartYear.productionStartYearSchema
      #+ FC.optional "episodesCount" episodesCount EpisodesCount.episodesCountSchema
      #+ FC.optional "productionCompany" productionCompany CompanyHeader.companyHeaderSchema
      #+ FC.optional "seasonsCount" seasonsCount SeasonsCount.seasonsCountSchema
      #+ FC.required "abbreviation" abbreviation Abbreviation.abbreviationSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate OriginalRunStartDate.originalRunStartDateSchema
      #+ FC.optional "originalBroadcaster" originalBroadcaster CompanyHeader.companyHeaderSchema
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount FeatureLengthEpisodesCount.featureLengthEpisodesCountSchema
      #+ FC.optional "productionEndYear" productionEndYear ProductionEndYear.productionEndYearSchema
      #+ FC.optional "originalRunEndDate" originalRunEndDate OriginalRunEndDate.originalRunEndDateSchema