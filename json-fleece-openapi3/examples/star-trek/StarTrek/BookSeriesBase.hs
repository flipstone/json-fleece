{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesBase
  ( BookSeriesBase(..)
  , bookSeriesBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data BookSeriesBase = BookSeriesBase
  { yearFrom :: Maybe Integer -- ^ Starting year of book series stories
  , numberOfBooks :: Maybe Integer -- ^ Number of pages
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the book series was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the book series was published
  , uid :: Text -- ^ Book series unique ID
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the book series was published
  , title :: Text -- ^ Book series title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the book series was published
  , miniseries :: Maybe Bool -- ^ Whether it's a miniseries
  , yearTo :: Maybe Integer -- ^ Ending year of book series stories
  , eBookSeries :: Maybe Bool -- ^ Whether it's a e-book series
  }
  deriving (Eq, Show)

bookSeriesBaseSchema :: FC.Fleece schema => schema BookSeriesBase
bookSeriesBaseSchema =
  FC.object $
    FC.constructor BookSeriesBase
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "numberOfBooks" numberOfBooks FC.integer
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optional "miniseries" miniseries FC.boolean
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "eBookSeries" eBookSeries FC.boolean