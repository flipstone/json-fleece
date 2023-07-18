{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesBase
  ( ComicSeriesBase(..)
  , comicSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicSeriesBase.Miniseries as Miniseries
import qualified StarTrek.Types.ComicSeriesBase.NumberOfIssues as NumberOfIssues
import qualified StarTrek.Types.ComicSeriesBase.PhotonovelSeries as PhotonovelSeries
import qualified StarTrek.Types.ComicSeriesBase.PublishedDayFrom as PublishedDayFrom
import qualified StarTrek.Types.ComicSeriesBase.PublishedDayTo as PublishedDayTo
import qualified StarTrek.Types.ComicSeriesBase.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.ComicSeriesBase.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.ComicSeriesBase.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.ComicSeriesBase.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.ComicSeriesBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.ComicSeriesBase.StardateTo as StardateTo
import qualified StarTrek.Types.ComicSeriesBase.Title as Title
import qualified StarTrek.Types.ComicSeriesBase.Uid as Uid
import qualified StarTrek.Types.ComicSeriesBase.YearFrom as YearFrom
import qualified StarTrek.Types.ComicSeriesBase.YearTo as YearTo

data ComicSeriesBase = ComicSeriesBase
  { yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic series stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic series stories
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic series was published
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , uid :: Uid.Uid -- ^ Comic series unique ID
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic series was published
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic series was published
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic series stories
  , photonovelSeries :: Maybe PhotonovelSeries.PhotonovelSeries -- ^ Whether it's a photonovel series
  , title :: Title.Title -- ^ Comic series title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic series was published
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesBaseSchema :: FC.Fleece schema => schema ComicSeriesBase
comicSeriesBaseSchema =
  FC.object $
    FC.constructor ComicSeriesBase
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "photonovelSeries" photonovelSeries PhotonovelSeries.photonovelSeriesSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema