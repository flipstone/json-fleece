{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase
  ( SeriesBase(..)
  , seriesBaseSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyHeader (CompanyHeader, companyHeaderSchema)

data SeriesBase = SeriesBase
  { productionCompany :: Maybe CompanyHeader -- ^ Header company, embedded in other objects
  , originalRunStartDate :: Maybe Day -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyHeader -- ^ Header company, embedded in other objects
  , productionStartYear :: Maybe Integer -- ^ Year the series production started
  , uid :: Text -- ^ Series unique ID
  , episodesCount :: Maybe Integer -- ^ Number of episodes
  , abbreviation :: Text -- ^ Series abbreviation
  , title :: Text -- ^ Series title
  , seasonsCount :: Maybe Integer -- ^ Number of seasons
  , featureLengthEpisodesCount :: Maybe Integer -- ^ Number of feature length episodes
  , productionEndYear :: Maybe Integer -- ^ Year the series production ended
  , originalRunEndDate :: Maybe Day -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesBaseSchema :: FC.Fleece schema => schema SeriesBase
seriesBaseSchema =
  FC.object $
    FC.constructor SeriesBase
      #+ FC.optional "productionCompany" productionCompany companyHeaderSchema
      #+ FC.optional "originalRunStartDate" originalRunStartDate FC.day
      #+ FC.optional "originalBroadcaster" originalBroadcaster companyHeaderSchema
      #+ FC.optional "productionStartYear" productionStartYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "episodesCount" episodesCount FC.integer
      #+ FC.required "abbreviation" abbreviation FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "seasonsCount" seasonsCount FC.integer
      #+ FC.optional "featureLengthEpisodesCount" featureLengthEpisodesCount FC.integer
      #+ FC.optional "productionEndYear" productionEndYear FC.integer
      #+ FC.optional "originalRunEndDate" originalRunEndDate FC.day