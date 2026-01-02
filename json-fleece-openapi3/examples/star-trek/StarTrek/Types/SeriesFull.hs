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
  { abbreviation :: Abbreviation.Abbreviation -- ^ Series abbreviation
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , episodesCount :: Maybe EpisodesCount.EpisodesCount -- ^ Number of episodes
  , featureLengthEpisodesCount :: Maybe FeatureLengthEpisodesCount.FeatureLengthEpisodesCount -- ^ Number of feature length episodes
  , originalBroadcaster :: Maybe CompanyBase.CompanyBase -- ^ Base company, returned in search results
  , originalRunEndDate :: Maybe OriginalRunEndDate.OriginalRunEndDate -- ^ Date the series originally ran to
  , originalRunStartDate :: Maybe OriginalRunStartDate.OriginalRunStartDate -- ^ Date the series originally ran from
  , productionCompany :: Maybe CompanyBase.CompanyBase -- ^ Base company, returned in search results
  , productionEndYear :: Maybe ProductionEndYear.ProductionEndYear -- ^ Year the series production ended
  , productionStartYear :: Maybe ProductionStartYear.ProductionStartYear -- ^ Year the series production started
  , seasons :: Maybe [SeasonBase.SeasonBase] -- ^ Base season, returned in search results
  , seasonsCount :: Maybe SeasonsCount.SeasonsCount -- ^ Number of seasons
  , title :: Title.Title -- ^ Series title
  , uid :: Uid.Uid -- ^ Series unique ID
  }
  deriving (Eq, Show)

seriesFullSchema :: FC.Fleece t => FC.Schema t SeriesFull
seriesFullSchema =
  FC.object $
    FC.constructor SeriesFull
      #+ FC.required "abbreviation" abbreviation Abbreviation.abbreviationSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "episodesCount" episodesCount EpisodesCount.episodesCountSchema
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount FeatureLengthEpisodesCount.featureLengthEpisodesCountSchema
      #+ FC.optional "originalBroadcaster" originalBroadcaster CompanyBase.companyBaseSchema
      #+ FC.optional "originalRunEndDate" originalRunEndDate OriginalRunEndDate.originalRunEndDateSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate OriginalRunStartDate.originalRunStartDateSchema
      #+ FC.optional "productionCompany" productionCompany CompanyBase.companyBaseSchema
      #+ FC.optional "productionEndYear" productionEndYear ProductionEndYear.productionEndYearSchema
      #+ FC.optional "productionStartYear" productionStartYear ProductionStartYear.productionStartYearSchema
      #+ FC.optional "seasons" seasons (FC.list SeasonBase.seasonBaseSchema)
      #+ FC.optional "seasonsCount" seasonsCount SeasonsCount.seasonsCountSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema