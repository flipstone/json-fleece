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
  { yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book series stories
  , eBookSeries :: Maybe EBookSeries.EBookSeries -- ^ Whether it's a e-book series
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the book series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the book series was published
  , uid :: Uid.Uid -- ^ Book series unique ID
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the book series was published
  , title :: Title.Title -- ^ Book series title
  , numberOfBooks :: Maybe NumberOfBooks.NumberOfBooks -- ^ Number of pages
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the book series was published
  }
  deriving (Eq, Show)

bookSeriesBaseSchema :: FC.Fleece schema => schema BookSeriesBase
bookSeriesBaseSchema =
  FC.object $
    FC.constructor BookSeriesBase
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "eBookSeries" eBookSeries EBookSeries.eBookSeriesSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "numberOfBooks" numberOfBooks NumberOfBooks.numberOfBooksSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema