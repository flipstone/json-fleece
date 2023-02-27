{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripBase
  ( ComicStripBase(..)
  , comicStripBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)

data ComicStripBase = ComicStripBase
  { yearFrom :: Maybe Integer -- ^ Starting year of comic strip story
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the comic strip was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the comic strip was published
  , uid :: Text -- ^ Comic strip unique ID
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the comic strip was published
  , title :: Text -- ^ Comic strip title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the comic strip was published
  , yearTo :: Maybe Integer -- ^ Ending year of comic strip story
  , periodical :: Maybe Text -- ^ Title of the periodical the comic strip was published in
  , publishedDayTo :: Maybe Integer -- ^ Day to which the comic strip was published
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , publishedDayFrom :: Maybe Integer -- ^ Day from which the comic strip was published
  }
  deriving (Eq, Show)

comicStripBaseSchema :: FC.Fleece schema => schema ComicStripBase
comicStripBaseSchema =
  FC.object $
    FC.constructor ComicStripBase
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "periodical" periodical FC.text
      #+ FC.optional "publishedDayTo" publishedDayTo FC.integer
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.optional "publishedDayFrom" publishedDayFrom FC.integer