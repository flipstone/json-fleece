{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesFull
  ( BookSeriesFull(..)
  , bookSeriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookBase (BookBase, bookBaseSchema)
import StarTrek.BookSeriesBase (BookSeriesBase, bookSeriesBaseSchema)
import StarTrek.BookSeriesFull.EBookSeries (EBookSeries, eBookSeriesSchema)
import StarTrek.BookSeriesFull.Miniseries (Miniseries, miniseriesSchema)
import StarTrek.BookSeriesFull.NumberOfBooks (NumberOfBooks, numberOfBooksSchema)
import StarTrek.BookSeriesFull.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.BookSeriesFull.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.BookSeriesFull.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.BookSeriesFull.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.BookSeriesFull.Title (Title, titleSchema)
import StarTrek.BookSeriesFull.Uid (Uid, uidSchema)
import StarTrek.BookSeriesFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.BookSeriesFull.YearTo (YearTo, yearToSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)

data BookSeriesFull = BookSeriesFull
  { yearFrom :: Maybe YearFrom -- ^ Starting year of book series stories
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , numberOfBooks :: Maybe NumberOfBooks -- ^ Number of books in book series
  , publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the book series was published
  , books :: Maybe [BookBase] -- ^ Base book, returned in search results
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the book series was published
  , uid :: Uid -- ^ Book series unique ID
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the book series was published
  , parentSeries :: Maybe [BookSeriesBase] -- ^ Base book series, returned in search results
  , title :: Title -- ^ Book series title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the book series was published
  , miniseries :: Maybe Miniseries -- ^ Whether it's a miniseries
  , yearTo :: Maybe YearTo -- ^ Ending year of book series stories
  , childSeries :: Maybe [BookSeriesBase] -- ^ Base book series, returned in search results
  , eBookSeries :: Maybe EBookSeries -- ^ Whether it's a e-book series
  }
  deriving (Eq, Show)

bookSeriesFullSchema :: FC.Fleece schema => schema BookSeriesFull
bookSeriesFullSchema =
  FC.object $
    FC.constructor BookSeriesFull
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "numberOfBooks" numberOfBooks numberOfBooksSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "books" books (FC.list bookBaseSchema)
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.optional "parentSeries" parentSeries (FC.list bookSeriesBaseSchema)
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema
      #+ FC.optional "miniseries" miniseries miniseriesSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "childSeries" childSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optional "eBookSeries" eBookSeries eBookSeriesSchema