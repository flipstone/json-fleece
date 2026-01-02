{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripFull
  ( ComicStripFull(..)
  , comicStripFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.ComicSeriesBase as ComicSeriesBase
import qualified StarTrek.Types.ComicStripFull.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.ComicStripFull.Periodical as Periodical
import qualified StarTrek.Types.ComicStripFull.PublishedDayFrom as PublishedDayFrom
import qualified StarTrek.Types.ComicStripFull.PublishedDayTo as PublishedDayTo
import qualified StarTrek.Types.ComicStripFull.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.ComicStripFull.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.ComicStripFull.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.ComicStripFull.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.ComicStripFull.Title as Title
import qualified StarTrek.Types.ComicStripFull.Uid as Uid
import qualified StarTrek.Types.ComicStripFull.YearFrom as YearFrom
import qualified StarTrek.Types.ComicStripFull.YearTo as YearTo
import qualified StarTrek.Types.StaffBase as StaffBase

data ComicStripFull = ComicStripFull
  { artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , comicSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , periodical :: Maybe Periodical.Periodical -- ^ Title of the periodical the comic strip was published in
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic strip was published
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic strip was published
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic strip was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic strip was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic strip was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic strip was published
  , title :: Title.Title -- ^ Comic strip title
  , uid :: Uid.Uid -- ^ Comic strip unique ID
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic strip stories
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic strip stories
  }
  deriving (Eq, Show)

comicStripFullSchema :: FC.Fleece t => FC.Schema t ComicStripFull
comicStripFullSchema =
  FC.object $
    FC.constructor ComicStripFull
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "comicSeries" comicSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
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
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema