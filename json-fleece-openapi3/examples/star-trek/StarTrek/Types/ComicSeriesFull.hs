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
  { yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic series stories
  , childSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic series stories
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic series was published
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , uid :: Uid.Uid -- ^ Comic series unique ID
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic series was published
  , parentSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , comics :: Maybe [ComicsBase.ComicsBase] -- ^ Base comics, returned in search results
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic series was published
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic series stories
  , photonovelSeries :: Maybe PhotonovelSeries.PhotonovelSeries -- ^ Whether it's a photonovel series
  , title :: Title.Title -- ^ Comic series title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic series was published
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesFullSchema :: FC.Fleece schema => schema ComicSeriesFull
comicSeriesFullSchema =
  FC.object $
    FC.constructor ComicSeriesFull
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "childSeries" childSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema
      #+ FC.optional "parentSeries" parentSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "comics" comics (FC.list ComicsBase.comicsBaseSchema)
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "photonovelSeries" photonovelSeries PhotonovelSeries.photonovelSeriesSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema