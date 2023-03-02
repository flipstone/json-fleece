{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesFull
  ( SeriesFull(..)
  , seriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CompanyBase as CompanyBase
import qualified StarTrek.EpisodeBase as EpisodeBase
import qualified StarTrek.SeasonBase as SeasonBase
import qualified StarTrek.SeriesFull.Abbreviation as Abbreviation
import qualified StarTrek.SeriesFull.EpisodesCount as EpisodesCount
import qualified StarTrek.SeriesFull.FeatureLengthEpisodesCount as FeatureLengthEpisodesCount
import qualified StarTrek.SeriesFull.OriginalRunEndDate as OriginalRunEndDate
import qualified StarTrek.SeriesFull.OriginalRunStartDate as OriginalRunStartDate
import qualified StarTrek.SeriesFull.ProductionEndYear as ProductionEndYear
import qualified StarTrek.SeriesFull.ProductionStartYear as ProductionStartYear
import qualified StarTrek.SeriesFull.SeasonsCount as SeasonsCount
import qualified StarTrek.SeriesFull.Title as Title
import qualified StarTrek.SeriesFull.Uid as Uid

data SeriesFull = SeriesFull
  { productionCompany :: Maybe CompanyBase.CompanyBase -- ^ Base company, returned in search results
  , originalRunStartDate :: Maybe OriginalRunStartDate.OriginalRunStartDate -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyBase.CompanyBase -- ^ Base company, returned in search results
  , productionStartYear :: Maybe ProductionStartYear.ProductionStartYear -- ^ Year the series production started
  , seasons :: Maybe [SeasonBase.SeasonBase] -- ^ Base season, returned in search results
  , uid :: Uid.Uid -- ^ Series unique ID
  , episodesCount :: Maybe EpisodesCount.EpisodesCount -- ^ Number of episodes
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , abbreviation :: Abbreviation.Abbreviation -- ^ Series abbreviation
  , title :: Title.Title -- ^ Series title
  , seasonsCount :: Maybe SeasonsCount.SeasonsCount -- ^ Number of seasons
  , featureLengthEpisodesCount :: Maybe FeatureLengthEpisodesCount.FeatureLengthEpisodesCount -- ^ Number of feature length episodes
  , productionEndYear :: Maybe ProductionEndYear.ProductionEndYear -- ^ Year the series production ended
  , originalRunEndDate :: Maybe OriginalRunEndDate.OriginalRunEndDate -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesFullSchema :: FC.Fleece schema => schema SeriesFull
seriesFullSchema =
  FC.object $
    FC.constructor SeriesFull
      #+ FC.optional "productionCompany" productionCompany CompanyBase.companyBaseSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate OriginalRunStartDate.originalRunStartDateSchema
      #+ FC.optional "originalBroadcaster" originalBroadcaster CompanyBase.companyBaseSchema
      #+ FC.optional "productionStartYear" productionStartYear ProductionStartYear.productionStartYearSchema
      #+ FC.optional "seasons" seasons (FC.list SeasonBase.seasonBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "episodesCount" episodesCount EpisodesCount.episodesCountSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.required "abbreviation" abbreviation Abbreviation.abbreviationSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "seasonsCount" seasonsCount SeasonsCount.seasonsCountSchema
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount FeatureLengthEpisodesCount.featureLengthEpisodesCountSchema
      #+ FC.optional "productionEndYear" productionEndYear ProductionEndYear.productionEndYearSchema
      #+ FC.optional "originalRunEndDate" originalRunEndDate OriginalRunEndDate.originalRunEndDateSchema