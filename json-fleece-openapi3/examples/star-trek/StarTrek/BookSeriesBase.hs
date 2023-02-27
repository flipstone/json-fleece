{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesBase
  ( BookSeriesBase(..)
  , bookSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookSeriesBase.EBookSeries (EBookSeries, eBookSeriesSchema)
import StarTrek.BookSeriesBase.Miniseries (Miniseries, miniseriesSchema)
import StarTrek.BookSeriesBase.NumberOfBooks (NumberOfBooks, numberOfBooksSchema)
import StarTrek.BookSeriesBase.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.BookSeriesBase.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.BookSeriesBase.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.BookSeriesBase.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.BookSeriesBase.Title (Title, titleSchema)
import StarTrek.BookSeriesBase.Uid (Uid, uidSchema)
import StarTrek.BookSeriesBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.BookSeriesBase.YearTo (YearTo, yearToSchema)

data BookSeriesBase = BookSeriesBase
  { yearFrom :: Maybe YearFrom -- ^ Starting year of book series stories
  , numberOfBooks :: Maybe NumberOfBooks -- ^ Number of pages
  , publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the book series was published
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the book series was published
  , uid :: Uid -- ^ Book series unique ID
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the book series was published
  , title :: Title -- ^ Book series title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the book series was published
  , miniseries :: Maybe Miniseries -- ^ Whether it's a miniseries
  , yearTo :: Maybe YearTo -- ^ Ending year of book series stories
  , eBookSeries :: Maybe EBookSeries -- ^ Whether it's a e-book series
  }
  deriving (Eq, Show)

bookSeriesBaseSchema :: FC.Fleece schema => schema BookSeriesBase
bookSeriesBaseSchema =
  FC.object $
    FC.constructor BookSeriesBase
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "numberOfBooks" numberOfBooks numberOfBooksSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema
      #+ FC.optional "miniseries" miniseries miniseriesSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "eBookSeries" eBookSeries eBookSeriesSchema