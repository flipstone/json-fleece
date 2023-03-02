{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesFull
  ( BookSeriesFull(..)
  , bookSeriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookBase as BookBase
import qualified StarTrek.BookSeriesBase as BookSeriesBase
import qualified StarTrek.BookSeriesFull.EBookSeries as EBookSeries
import qualified StarTrek.BookSeriesFull.Miniseries as Miniseries
import qualified StarTrek.BookSeriesFull.NumberOfBooks as NumberOfBooks
import qualified StarTrek.BookSeriesFull.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.BookSeriesFull.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.BookSeriesFull.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.BookSeriesFull.PublishedYearTo as PublishedYearTo
import qualified StarTrek.BookSeriesFull.Title as Title
import qualified StarTrek.BookSeriesFull.Uid as Uid
import qualified StarTrek.BookSeriesFull.YearFrom as YearFrom
import qualified StarTrek.BookSeriesFull.YearTo as YearTo
import qualified StarTrek.CompanyBase as CompanyBase

data BookSeriesFull = BookSeriesFull
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book series stories
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , numberOfBooks :: Maybe NumberOfBooks.NumberOfBooks -- ^ Number of books in book series
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the book series was published
  , books :: Maybe [BookBase.BookBase] -- ^ Base book, returned in search results
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the book series was published
  , uid :: Uid.Uid -- ^ Book series unique ID
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the book series was published
  , parentSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , title :: Title.Title -- ^ Book series title
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the book series was published
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book series stories
  , childSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , eBookSeries :: Maybe EBookSeries.EBookSeries -- ^ Whether it's a e-book series
  }
  deriving (Eq, Show)

bookSeriesFullSchema :: FC.Fleece schema => schema BookSeriesFull
bookSeriesFullSchema =
  FC.object $
    FC.constructor BookSeriesFull
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "numberOfBooks" numberOfBooks NumberOfBooks.numberOfBooksSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "books" books (FC.list BookBase.bookBaseSchema)
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "parentSeries" parentSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "childSeries" childSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.optional "eBookSeries" eBookSeries EBookSeries.eBookSeriesSchema