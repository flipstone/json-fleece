{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesFull
  ( SeriesFull(..)
  , seriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.EpisodeBase as EpisodeBase
import qualified StarTrek.Types.SeasonBase as SeasonBase
import qualified StarTrek.Types.SeriesFull.Abbreviation as Abbreviation
import qualified StarTrek.Types.SeriesFull.EpisodesCount as EpisodesCount
import qualified StarTrek.Types.SeriesFull.FeatureLengthEpisodesCount as FeatureLengthEpisodesCount
import qualified StarTrek.Types.SeriesFull.OriginalRunEndDate as OriginalRunEndDate
import qualified StarTrek.Types.SeriesFull.OriginalRunStartDate as OriginalRunStartDate
import qualified StarTrek.Types.SeriesFull.ProductionEndYear as ProductionEndYear
import qualified StarTrek.Types.SeriesFull.ProductionStartYear as ProductionStartYear
import qualified StarTrek.Types.SeriesFull.SeasonsCount as SeasonsCount
import qualified StarTrek.Types.SeriesFull.Title as Title
import qualified StarTrek.Types.SeriesFull.Uid as Uid

data SeriesFull = SeriesFull
  { title :: Title.Title -- ^ Series title
  , productionStartYear :: Maybe ProductionStartYear.ProductionStartYear -- ^ Year the series production started
  , episodesCount :: Maybe EpisodesCount.EpisodesCount -- ^ Number of episodes
  , productionCompany :: Maybe CompanyBase.CompanyBase -- ^ Base company, returned in search results
  , seasonsCount :: Maybe SeasonsCount.SeasonsCount -- ^ Number of seasons
  , abbreviation :: Abbreviation.Abbreviation -- ^ Series abbreviation
  , seasons :: Maybe [SeasonBase.SeasonBase] -- ^ Base season, returned in search results
  , uid :: Uid.Uid -- ^ Series unique ID
  , originalRunStartDate :: Maybe OriginalRunStartDate.OriginalRunStartDate -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyBase.CompanyBase -- ^ Base company, returned in search results
  , featureLengthEpisodesCount :: Maybe FeatureLengthEpisodesCount.FeatureLengthEpisodesCount -- ^ Number of feature length episodes
  , productionEndYear :: Maybe ProductionEndYear.ProductionEndYear -- ^ Year the series production ended
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , originalRunEndDate :: Maybe OriginalRunEndDate.OriginalRunEndDate -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesFullSchema :: FC.Fleece schema => schema SeriesFull
seriesFullSchema =
  FC.object $
    FC.constructor SeriesFull
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "productionStartYear" productionStartYear ProductionStartYear.productionStartYearSchema
      #+ FC.optional "episodesCount" episodesCount EpisodesCount.episodesCountSchema
      #+ FC.optional "productionCompany" productionCompany CompanyBase.companyBaseSchema
      #+ FC.optional "seasonsCount" seasonsCount SeasonsCount.seasonsCountSchema
      #+ FC.required "abbreviation" abbreviation Abbreviation.abbreviationSchema
      #+ FC.optional "seasons" seasons (FC.list SeasonBase.seasonBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate OriginalRunStartDate.originalRunStartDateSchema
      #+ FC.optional "originalBroadcaster" originalBroadcaster CompanyBase.companyBaseSchema
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount FeatureLengthEpisodesCount.featureLengthEpisodesCountSchema
      #+ FC.optional "productionEndYear" productionEndYear ProductionEndYear.productionEndYearSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "originalRunEndDate" originalRunEndDate OriginalRunEndDate.originalRunEndDateSchema