{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase
  ( SeriesBase(..)
  , seriesBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyHeader (CompanyHeader, companyHeaderSchema)

data SeriesBase = SeriesBase
  { productionCompany :: Maybe CompanyHeader -- ^ Header company, embedded in other objects
  , originalRunStartDate :: Maybe Text -- ^ Date the series originally ran from
  , originalBroadcaster :: Maybe CompanyHeader -- ^ Header company, embedded in other objects
  , productionStartYear :: Maybe Integer -- ^ Year the series production started
  , uid :: Text -- ^ Series unique ID
  , episodesCount :: Maybe Integer -- ^ Number of episodes
  , abbreviation :: Text -- ^ Series abbreviation
  , title :: Text -- ^ Series title
  , seasonsCount :: Maybe Integer -- ^ Number of seasons
  , featureLengthEpisodesCount :: Maybe Integer -- ^ Number of feature length episodes
  , productionEndYear :: Maybe Integer -- ^ Year the series production ended
  , originalRunEndDate :: Maybe Text -- ^ Date the series originally ran to
  }
  deriving (Eq, Show)

seriesBaseSchema :: FC.Fleece schema => schema SeriesBase
seriesBaseSchema =
  FC.object $
    FC.constructor SeriesBase
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionCompany" productionCompany companyHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "originalRunStartDate" originalRunStartDate FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "originalBroadcaster" originalBroadcaster companyHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionStartYear" productionStartYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "episodesCount" episodesCount FC.integer
      #+ FC.required "abbreviation" abbreviation FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "seasonsCount" seasonsCount FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "featureLengthEpisodesCount" featureLengthEpisodesCount FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "productionEndYear" productionEndYear FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "originalRunEndDate" originalRunEndDate FC.text