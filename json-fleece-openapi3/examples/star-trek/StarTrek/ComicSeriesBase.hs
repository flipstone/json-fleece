{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesBase
  ( ComicSeriesBase(..)
  , comicSeriesBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Float, Integer, Maybe, Show)

data ComicSeriesBase = ComicSeriesBase
  { yearFrom :: Maybe Integer -- ^ Starting year of comic series stories
  , stardateTo :: Maybe Float -- ^ Ending stardate of comic series stories
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the comic series was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the comic series was published
  , uid :: Text -- ^ Comic series unique ID
  , stardateFrom :: Maybe Float -- ^ Starting stardate of comic series stories
  , numberOfIssues :: Maybe Integer -- ^ Number of issues
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the comic series was published
  , title :: Text -- ^ Comic series title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the comic series was published
  , photonovelSeries :: Maybe Bool -- ^ Whether it's a photonovel series
  , miniseries :: Maybe Bool -- ^ Whether it's a miniseries
  , yearTo :: Maybe Integer -- ^ Ending year of comic series stories
  , publishedDayTo :: Maybe Integer -- ^ Day to which the comic series was published
  , publishedDayFrom :: Maybe Integer -- ^ Day from which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesBaseSchema :: FC.Fleece schema => schema ComicSeriesBase
comicSeriesBaseSchema =
  FC.object $
    FC.constructor ComicSeriesBase
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.float
      #+ FC.optional "numberOfIssues" numberOfIssues FC.integer
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optional "photonovelSeries" photonovelSeries FC.boolean
      #+ FC.optional "miniseries" miniseries FC.boolean
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "publishedDayTo" publishedDayTo FC.integer
      #+ FC.optional "publishedDayFrom" publishedDayFrom FC.integer