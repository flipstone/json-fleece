{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesBase
  ( ComicSeriesBase(..)
  , comicSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ComicSeriesBase.Miniseries as Miniseries
import qualified StarTrek.ComicSeriesBase.NumberOfIssues as NumberOfIssues
import qualified StarTrek.ComicSeriesBase.PhotonovelSeries as PhotonovelSeries
import qualified StarTrek.ComicSeriesBase.PublishedDayFrom as PublishedDayFrom
import qualified StarTrek.ComicSeriesBase.PublishedDayTo as PublishedDayTo
import qualified StarTrek.ComicSeriesBase.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.ComicSeriesBase.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.ComicSeriesBase.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.ComicSeriesBase.PublishedYearTo as PublishedYearTo
import qualified StarTrek.ComicSeriesBase.StardateFrom as StardateFrom
import qualified StarTrek.ComicSeriesBase.StardateTo as StardateTo
import qualified StarTrek.ComicSeriesBase.Title as Title
import qualified StarTrek.ComicSeriesBase.Uid as Uid
import qualified StarTrek.ComicSeriesBase.YearFrom as YearFrom
import qualified StarTrek.ComicSeriesBase.YearTo as YearTo

data ComicSeriesBase = ComicSeriesBase
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic series stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic series was published
  , uid :: Uid.Uid -- ^ Comic series unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic series stories
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic series was published
  , title :: Title.Title -- ^ Comic series title
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic series was published
  , photonovelSeries :: Maybe PhotonovelSeries.PhotonovelSeries -- ^ Whether it's a photonovel series
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic series stories
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic series was published
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesBaseSchema :: FC.Fleece schema => schema ComicSeriesBase
comicSeriesBaseSchema =
  FC.object $
    FC.constructor ComicSeriesBase
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "photonovelSeries" photonovelSeries PhotonovelSeries.photonovelSeriesSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema