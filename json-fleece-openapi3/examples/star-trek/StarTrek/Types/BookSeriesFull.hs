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
  { title :: Title.Title -- ^ Book series title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the book series was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the book series was published
  , books :: Maybe [BookBase.BookBase] -- ^ Base book, returned in search results
  , childSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , uid :: Uid.Uid -- ^ Book series unique ID
  , numberOfBooks :: Maybe NumberOfBooks.NumberOfBooks -- ^ Number of books in book series
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , miniseries :: Maybe Miniseries.Miniseries -- ^ Whether it's a miniseries
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the book series was published
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book series stories
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the book series was published
  , parentSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , eBookSeries :: Maybe EBookSeries.EBookSeries -- ^ Whether it's a e-book series
  }
  deriving (Eq, Show)

bookSeriesFullSchema :: FC.Fleece schema => schema BookSeriesFull
bookSeriesFullSchema =
  FC.object $
    FC.constructor BookSeriesFull
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "books" books (FC.list BookBase.bookBaseSchema)
      #+ FC.optional "childSeries" childSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "numberOfBooks" numberOfBooks NumberOfBooks.numberOfBooksSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "miniseries" miniseries Miniseries.miniseriesSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "parentSeries" parentSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.optional "eBookSeries" eBookSeries EBookSeries.eBookSeriesSchema