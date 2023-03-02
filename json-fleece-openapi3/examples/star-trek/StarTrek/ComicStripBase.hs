{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripBase
  ( ComicStripBase(..)
  , comicStripBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ComicStripBase.NumberOfPages as NumberOfPages
import qualified StarTrek.ComicStripBase.Periodical as Periodical
import qualified StarTrek.ComicStripBase.PublishedDayFrom as PublishedDayFrom
import qualified StarTrek.ComicStripBase.PublishedDayTo as PublishedDayTo
import qualified StarTrek.ComicStripBase.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.ComicStripBase.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.ComicStripBase.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.ComicStripBase.PublishedYearTo as PublishedYearTo
import qualified StarTrek.ComicStripBase.Title as Title
import qualified StarTrek.ComicStripBase.Uid as Uid
import qualified StarTrek.ComicStripBase.YearFrom as YearFrom
import qualified StarTrek.ComicStripBase.YearTo as YearTo

data ComicStripBase = ComicStripBase
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic strip story
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic strip was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic strip was published
  , uid :: Uid.Uid -- ^ Comic strip unique ID
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic strip was published
  , title :: Title.Title -- ^ Comic strip title
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic strip was published
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic strip story
  , periodical :: Maybe Periodical.Periodical -- ^ Title of the periodical the comic strip was published in
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic strip was published
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic strip was published
  }
  deriving (Eq, Show)

comicStripBaseSchema :: FC.Fleece schema => schema ComicStripBase
comicStripBaseSchema =
  FC.object $
    FC.constructor ComicStripBase
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "periodical" periodical Periodical.periodicalSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema