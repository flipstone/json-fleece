{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesFull
  ( ComicSeriesFull(..)
  , comicSeriesFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Float, Integer, Maybe, Show)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicsBase (ComicsBase, comicsBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)

data ComicSeriesFull = ComicSeriesFull
  { yearFrom :: Maybe Integer -- ^ Starting year of comic series stories
  , stardateTo :: Maybe Float -- ^ Ending stardate of comic series stories
  , publishers :: Maybe [CompanyBase] -- ^ Companies that published this comic series
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the comic series was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the comic series was published
  , uid :: Text -- ^ Comic series unique ID
  , stardateFrom :: Maybe Float -- ^ Starting stardate of comic series stories
  , numberOfIssues :: Maybe Integer -- ^ Number of issues
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the comic series was published
  , parentSeries :: Maybe [ComicSeriesBase] -- ^ Comic series this comic series is included in
  , title :: Text -- ^ Comic series title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the comic series was published
  , photonovelSeries :: Maybe Bool -- ^ Whether it's a photonovel series
  , miniseries :: Maybe Bool -- ^ Whether it's a miniseries
  , yearTo :: Maybe Integer -- ^ Ending year of comic series stories
  , childSeries :: Maybe [ComicSeriesBase] -- ^ Child comic series included in this comic series
  , comics :: Maybe [ComicsBase] -- ^ Comics included in this comic series
  , publishedDayTo :: Maybe Integer -- ^ Day to which the comic series was published
  , publishedDayFrom :: Maybe Integer -- ^ Day from which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesFullSchema :: FC.Fleece schema => schema ComicSeriesFull
comicSeriesFullSchema =
  FC.object $
    FC.constructor ComicSeriesFull
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.float
      #+ FC.optional "numberOfIssues" numberOfIssues FC.integer
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.optional "parentSeries" parentSeries (FC.list comicSeriesBaseSchema)
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optional "photonovelSeries" photonovelSeries FC.boolean
      #+ FC.optional "miniseries" miniseries FC.boolean
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "childSeries" childSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optional "comics" comics (FC.list comicsBaseSchema)
      #+ FC.optional "publishedDayTo" publishedDayTo FC.integer
      #+ FC.optional "publishedDayFrom" publishedDayFrom FC.integer