{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesFull
  ( BookSeriesFull(..)
  , bookSeriesFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.BookBase (BookBase, bookBaseSchema)
import StarTrek.BookSeriesBase (BookSeriesBase, bookSeriesBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)

data BookSeriesFull = BookSeriesFull
  { yearFrom :: Maybe Integer -- ^ Starting year of book series stories
  , publishers :: Maybe [CompanyBase] -- ^ Companies that published this book series
  , numberOfBooks :: Maybe Integer -- ^ Number of books in book series
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the book series was published
  , books :: Maybe [BookBase] -- ^ Books included in this book series
  , publishedYearTo :: Maybe Integer -- ^ Year to which the book series was published
  , uid :: Text -- ^ Book series unique ID
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the book series was published
  , parentSeries :: Maybe [BookSeriesBase] -- ^ Book series this book series is included in
  , title :: Text -- ^ Book series title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the book series was published
  , miniseries :: Maybe Bool -- ^ Whether it's a miniseries
  , yearTo :: Maybe Integer -- ^ Ending year of book series stories
  , childSeries :: Maybe [BookSeriesBase] -- ^ Child book series included in this book series
  , eBookSeries :: Maybe Bool -- ^ Whether it's a e-book series
  }
  deriving (Eq, Show)

bookSeriesFullSchema :: FC.Fleece schema => schema BookSeriesFull
bookSeriesFullSchema =
  FC.object $
    FC.constructor BookSeriesFull
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "numberOfBooks" numberOfBooks FC.integer
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "books" books (FC.list bookBaseSchema)
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.optional "parentSeries" parentSeries (FC.list bookSeriesBaseSchema)
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optional "miniseries" miniseries FC.boolean
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "childSeries" childSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optional "eBookSeries" eBookSeries FC.boolean