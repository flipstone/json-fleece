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
  { title :: Title.Title -- ^ Comic series title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic series was published
  , photonovelSeries :: Maybe PhotonovelSeries.PhotonovelSeries -- ^ Whether it's a photonovel series
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic series was published
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic series was published
  , uid :: Uid.Uid -- ^ Comic series unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic series stories
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic series was published
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic series stories
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic series was published
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic series stories
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesBaseSchema :: FC.Fleece schema => schema ComicSeriesBase
comicSeriesBaseSchema =
  FC.object $
    FC.constructor ComicSeriesBase
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "photonovelSeries" photonovelSeries PhotonovelSeries.photonovelSeriesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema