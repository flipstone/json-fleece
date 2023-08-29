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
  { title :: Title.Title -- ^ Comic series title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic series was published
  , photonovelSeries :: Maybe PhotonovelSeries.PhotonovelSeries -- ^ Whether it's a photonovel series
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic series was published
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic series was published
  , childSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , uid :: Uid.Uid -- ^ Comic series unique ID
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic series stories
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic series was published
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic series stories
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic series was published
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic series stories
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic series was published
  , comics :: Maybe [ComicsBase.ComicsBase] -- ^ Base comics, returned in search results
  , parentSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  }
  deriving (Eq, Show)

comicSeriesFullSchema :: FC.Fleece schema => schema ComicSeriesFull
comicSeriesFullSchema =
  FC.object $
    FC.constructor ComicSeriesFull
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "photonovelSeries" photonovelSeries PhotonovelSeries.photonovelSeriesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema
      #+ FC.optional "childSeries" childSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "comics" comics (FC.list ComicsBase.comicsBaseSchema)
      #+ FC.optional "parentSeries" parentSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)