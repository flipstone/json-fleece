{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesBase
  ( BookSeriesBase(..)
  , bookSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookSeriesBase.EBookSeries as EBookSeries
import qualified StarTrek.Types.BookSeriesBase.Miniseries as Miniseries
import qualified StarTrek.Types.BookSeriesBase.NumberOfBooks as NumberOfBooks
import qualified StarTrek.Types.BookSeriesBase.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.BookSeriesBase.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.BookSeriesBase.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.BookSeriesBase.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.BookSeriesBase.Title as Title
import qualified StarTrek.Types.BookSeriesBase.Uid as Uid
import qualified StarTrek.Types.BookSeriesBase.YearFrom as YearFrom
import qualified StarTrek.Types.BookSeriesBase.YearTo as YearTo

data BookSeriesBase = BookSeriesBase
  { eBookSeries :: Maybe EBookSeries.EBookSeries -- ^ Whether it's a e-book series
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , numberOfBooks :: Maybe NumberOfBooks.NumberOfBooks -- ^ Number of pages
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the book series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the book series was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the book series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the book series was published
  , title :: Title.Title -- ^ Book series title
  , uid :: Uid.Uid -- ^ Book series unique ID
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book series stories
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book series stories
  }
  deriving (Eq, Show)

bookSeriesBaseSchema :: FC.Fleece schema => schema BookSeriesBase
bookSeriesBaseSchema =
  FC.object $
    FC.constructor BookSeriesBase
      #+ FC.optional "eBookSeries" eBookSeries EBookSeries.eBookSeriesSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "numberOfBooks" numberOfBooks NumberOfBooks.numberOfBooksSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema