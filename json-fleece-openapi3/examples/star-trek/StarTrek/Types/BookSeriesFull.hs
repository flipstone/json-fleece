{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesFull
  ( BookSeriesFull(..)
  , bookSeriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookBase as BookBase
import qualified StarTrek.Types.BookSeriesBase as BookSeriesBase
import qualified StarTrek.Types.BookSeriesFull.EBookSeries as EBookSeries
import qualified StarTrek.Types.BookSeriesFull.Miniseries as Miniseries
import qualified StarTrek.Types.BookSeriesFull.NumberOfBooks as NumberOfBooks
import qualified StarTrek.Types.BookSeriesFull.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.BookSeriesFull.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.BookSeriesFull.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.BookSeriesFull.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.BookSeriesFull.Title as Title
import qualified StarTrek.Types.BookSeriesFull.Uid as Uid
import qualified StarTrek.Types.BookSeriesFull.YearFrom as YearFrom
import qualified StarTrek.Types.BookSeriesFull.YearTo as YearTo
import qualified StarTrek.Types.CompanyBase as CompanyBase

data BookSeriesFull = BookSeriesFull
  { books :: Maybe [BookBase.BookBase] -- ^ Base book, returned in search results
  , childSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , eBookSeries :: Maybe EBookSeries.EBookSeries -- ^ Whether it's a e-book series
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , numberOfBooks :: Maybe NumberOfBooks.NumberOfBooks -- ^ Number of books in book series
  , parentSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the book series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the book series was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the book series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the book series was published
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , title :: Title.Title -- ^ Book series title
  , uid :: Uid.Uid -- ^ Book series unique ID
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book series stories
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book series stories
  }
  deriving (Eq, Show)

bookSeriesFullSchema :: FC.Fleece schema => schema BookSeriesFull
bookSeriesFullSchema =
  FC.object $
    FC.constructor BookSeriesFull
      #+ FC.optional "books" books (FC.list BookBase.bookBaseSchema)
      #+ FC.optional "childSeries" childSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.optional "eBookSeries" eBookSeries EBookSeries.eBookSeriesSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "numberOfBooks" numberOfBooks NumberOfBooks.numberOfBooksSchema
      #+ FC.optional "parentSeries" parentSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema