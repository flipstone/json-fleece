{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesFull
  ( SeriesFull(..)
  , seriesFullSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.SeasonBase (SeasonBase, seasonBaseSchema)

data SeriesFull = SeriesFull
  { productionCompany :: Maybe CompanyBase -- ^ Base company, returned in search results
  , originalRunStartDate :: Maybe Day -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyBase -- ^ Base company, returned in search results
  , productionStartYear :: Maybe Integer -- ^ Year the series production started
  , seasons :: Maybe [SeasonBase] -- ^ Seasons in the series
  , uid :: Text -- ^ Series unique ID
  , episodesCount :: Maybe Integer -- ^ Number of episodes
  , episodes :: Maybe [EpisodeBase] -- ^ Episodes in the series
  , abbreviation :: Text -- ^ Series abbreviation
  , title :: Text -- ^ Series title
  , seasonsCount :: Maybe Integer -- ^ Number of seasons
  , featureLengthEpisodesCount :: Maybe Integer -- ^ Number of feature length episodes
  , productionEndYear :: Maybe Integer -- ^ Year the series production ended
  , originalRunEndDate :: Maybe Day -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesFullSchema :: FC.Fleece schema => schema SeriesFull
seriesFullSchema =
  FC.object $
    FC.constructor SeriesFull
      #+ FC.optional "productionCompany" productionCompany companyBaseSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate FC.day
      #+ FC.optional "originalBroadcaster" originalBroadcaster companyBaseSchema
      #+ FC.optional "productionStartYear" productionStartYear FC.integer
      #+ FC.optional "seasons" seasons (FC.list seasonBaseSchema)
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "episodesCount" episodesCount FC.integer
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.required "abbreviation" abbreviation FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "seasonsCount" seasonsCount FC.integer
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount FC.integer
      #+ FC.optional "productionEndYear" productionEndYear FC.integer
      #+ FC.optional "originalRunEndDate" originalRunEndDate FC.day