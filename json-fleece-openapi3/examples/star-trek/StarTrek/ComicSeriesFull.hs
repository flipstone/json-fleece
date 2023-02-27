{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesFull
  ( ComicSeriesFull(..)
  , comicSeriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicSeriesFull.Miniseries (Miniseries, miniseriesSchema)
import StarTrek.ComicSeriesFull.NumberOfIssues (NumberOfIssues, numberOfIssuesSchema)
import StarTrek.ComicSeriesFull.PhotonovelSeries (PhotonovelSeries, photonovelSeriesSchema)
import StarTrek.ComicSeriesFull.PublishedDayFrom (PublishedDayFrom, publishedDayFromSchema)
import StarTrek.ComicSeriesFull.PublishedDayTo (PublishedDayTo, publishedDayToSchema)
import StarTrek.ComicSeriesFull.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.ComicSeriesFull.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.ComicSeriesFull.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.ComicSeriesFull.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.ComicSeriesFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.ComicSeriesFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.ComicSeriesFull.Title (Title, titleSchema)
import StarTrek.ComicSeriesFull.Uid (Uid, uidSchema)
import StarTrek.ComicSeriesFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicSeriesFull.YearTo (YearTo, yearToSchema)
import StarTrek.ComicsBase (ComicsBase, comicsBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)

data ComicSeriesFull = ComicSeriesFull
  { yearFrom :: Maybe YearFrom -- ^ Starting year of comic series stories
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of comic series stories
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the comic series was published
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the comic series was published
  , uid :: Uid -- ^ Comic series unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of comic series stories
  , numberOfIssues :: Maybe NumberOfIssues -- ^ Number of issues
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the comic series was published
  , parentSeries :: Maybe [ComicSeriesBase] -- ^ Base comic series, returned in search results
  , title :: Title -- ^ Comic series title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the comic series was published
  , photonovelSeries :: Maybe PhotonovelSeries -- ^ Whether it's a photonovel series
  , miniseries :: Maybe Miniseries -- ^ Whether it's a miniseries
  , yearTo :: Maybe YearTo -- ^ Ending year of comic series stories
  , childSeries :: Maybe [ComicSeriesBase] -- ^ Base comic series, returned in search results
  , comics :: Maybe [ComicsBase] -- ^ Base comics, returned in search results
  , publishedDayTo :: Maybe PublishedDayTo -- ^ Day to which the comic series was published
  , publishedDayFrom :: Maybe PublishedDayFrom -- ^ Day from which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesFullSchema :: FC.Fleece schema => schema ComicSeriesFull
comicSeriesFullSchema =
  FC.object $
    FC.constructor ComicSeriesFull
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "numberOfIssues" numberOfIssues numberOfIssuesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.optional "parentSeries" parentSeries (FC.list comicSeriesBaseSchema)
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema
      #+ FC.optional "photonovelSeries" photonovelSeries photonovelSeriesSchema
      #+ FC.optional "miniseries" miniseries miniseriesSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "childSeries" childSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optional "comics" comics (FC.list comicsBaseSchema)
      #+ FC.optional "publishedDayTo" publishedDayTo publishedDayToSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom publishedDayFromSchema