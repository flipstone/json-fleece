{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesFull
  ( ComicSeriesFull(..)
  , comicSeriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicSeriesBase as ComicSeriesBase
import qualified StarTrek.Types.ComicSeriesFull.Miniseries as Miniseries
import qualified StarTrek.Types.ComicSeriesFull.NumberOfIssues as NumberOfIssues
import qualified StarTrek.Types.ComicSeriesFull.PhotonovelSeries as PhotonovelSeries
import qualified StarTrek.Types.ComicSeriesFull.PublishedDayFrom as PublishedDayFrom
import qualified StarTrek.Types.ComicSeriesFull.PublishedDayTo as PublishedDayTo
import qualified StarTrek.Types.ComicSeriesFull.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.ComicSeriesFull.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.ComicSeriesFull.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.ComicSeriesFull.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.ComicSeriesFull.StardateFrom as StardateFrom
import qualified StarTrek.Types.ComicSeriesFull.StardateTo as StardateTo
import qualified StarTrek.Types.ComicSeriesFull.Title as Title
import qualified StarTrek.Types.ComicSeriesFull.Uid as Uid
import qualified StarTrek.Types.ComicSeriesFull.YearFrom as YearFrom
import qualified StarTrek.Types.ComicSeriesFull.YearTo as YearTo
import qualified StarTrek.Types.ComicsBase as ComicsBase
import qualified StarTrek.Types.CompanyBase as CompanyBase

data ComicSeriesFull = ComicSeriesFull
  { childSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , comics :: Maybe [ComicsBase.ComicsBase] -- ^ Base comics, returned in search results
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , parentSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , photonovelSeries :: Maybe PhotonovelSeries.PhotonovelSeries -- ^ Whether it's a photonovel series
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic series was published
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic series was published
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic series was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic series was published
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic series stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic series stories
  , title :: Title.Title -- ^ Comic series title
  , uid :: Uid.Uid -- ^ Comic series unique ID
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic series stories
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic series stories
  }
  deriving (Eq, Show)

comicSeriesFullSchema :: FC.Fleece t => FC.Schema t ComicSeriesFull
comicSeriesFullSchema =
  FC.object $
    FC.constructor ComicSeriesFull
      #+ FC.optional "childSeries" childSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "comics" comics (FC.list ComicsBase.comicsBaseSchema)
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "parentSeries" parentSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "photonovelSeries" photonovelSeries PhotonovelSeries.photonovelSeriesSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema