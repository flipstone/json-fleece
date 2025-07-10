{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripBase
  ( ComicStripBase(..)
  , comicStripBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicStripBase.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.ComicStripBase.Periodical as Periodical
import qualified StarTrek.Types.ComicStripBase.PublishedDayFrom as PublishedDayFrom
import qualified StarTrek.Types.ComicStripBase.PublishedDayTo as PublishedDayTo
import qualified StarTrek.Types.ComicStripBase.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.ComicStripBase.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.ComicStripBase.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.ComicStripBase.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.ComicStripBase.Title as Title
import qualified StarTrek.Types.ComicStripBase.Uid as Uid
import qualified StarTrek.Types.ComicStripBase.YearFrom as YearFrom
import qualified StarTrek.Types.ComicStripBase.YearTo as YearTo

data ComicStripBase = ComicStripBase
  { numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , periodical :: Maybe Periodical.Periodical -- ^ Title of the periodical the comic strip was published in
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic strip was published
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic strip was published
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic strip was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic strip was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic strip was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic strip was published
  , title :: Title.Title -- ^ Comic strip title
  , uid :: Uid.Uid -- ^ Comic strip unique ID
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic strip story
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic strip story
  }
  deriving (Eq, Show)

comicStripBaseSchema :: FC.Fleece schema => schema ComicStripBase
comicStripBaseSchema =
  FC.object $
    FC.constructor ComicStripBase
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "periodical" periodical Periodical.periodicalSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema