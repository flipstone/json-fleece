{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesFull
  ( SeriesFull(..)
  , seriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.SeasonBase (SeasonBase, seasonBaseSchema)
import StarTrek.SeriesFull.Abbreviation (Abbreviation, abbreviationSchema)
import StarTrek.SeriesFull.EpisodesCount (EpisodesCount, episodesCountSchema)
import StarTrek.SeriesFull.FeatureLengthEpisodesCount (FeatureLengthEpisodesCount, featureLengthEpisodesCountSchema)
import StarTrek.SeriesFull.OriginalRunEndDate (OriginalRunEndDate, originalRunEndDateSchema)
import StarTrek.SeriesFull.OriginalRunStartDate (OriginalRunStartDate, originalRunStartDateSchema)
import StarTrek.SeriesFull.ProductionEndYear (ProductionEndYear, productionEndYearSchema)
import StarTrek.SeriesFull.ProductionStartYear (ProductionStartYear, productionStartYearSchema)
import StarTrek.SeriesFull.SeasonsCount (SeasonsCount, seasonsCountSchema)
import StarTrek.SeriesFull.Title (Title, titleSchema)
import StarTrek.SeriesFull.Uid (Uid, uidSchema)

data SeriesFull = SeriesFull
  { productionCompany :: Maybe CompanyBase -- ^ Base company, returned in search results
  , originalRunStartDate :: Maybe OriginalRunStartDate -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyBase -- ^ Base company, returned in search results
  , productionStartYear :: Maybe ProductionStartYear -- ^ Year the series production started
  , seasons :: Maybe [SeasonBase] -- ^ Base season, returned in search results
  , uid :: Uid -- ^ Series unique ID
  , episodesCount :: Maybe EpisodesCount -- ^ Number of episodes
  , episodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , abbreviation :: Abbreviation -- ^ Series abbreviation
  , title :: Title -- ^ Series title
  , seasonsCount :: Maybe SeasonsCount -- ^ Number of seasons
  , featureLengthEpisodesCount :: Maybe FeatureLengthEpisodesCount -- ^ Number of feature length episodes
  , productionEndYear :: Maybe ProductionEndYear -- ^ Year the series production ended
  , originalRunEndDate :: Maybe OriginalRunEndDate -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesFullSchema :: FC.Fleece schema => schema SeriesFull
seriesFullSchema =
  FC.object $
    FC.constructor SeriesFull
      #+ FC.optional "productionCompany" productionCompany companyBaseSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate originalRunStartDateSchema
      #+ FC.optional "originalBroadcaster" originalBroadcaster companyBaseSchema
      #+ FC.optional "productionStartYear" productionStartYear productionStartYearSchema
      #+ FC.optional "seasons" seasons (FC.list seasonBaseSchema)
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "episodesCount" episodesCount episodesCountSchema
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.required "abbreviation" abbreviation abbreviationSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "seasonsCount" seasonsCount seasonsCountSchema
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount featureLengthEpisodesCountSchema
      #+ FC.optional "productionEndYear" productionEndYear productionEndYearSchema
      #+ FC.optional "originalRunEndDate" originalRunEndDate originalRunEndDateSchema