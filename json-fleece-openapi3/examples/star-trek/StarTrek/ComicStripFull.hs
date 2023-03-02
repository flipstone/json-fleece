{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripFull
  ( ComicStripFull(..)
  , comicStripFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CharacterBase as CharacterBase
import qualified StarTrek.ComicSeriesBase as ComicSeriesBase
import qualified StarTrek.ComicStripFull.NumberOfPages as NumberOfPages
import qualified StarTrek.ComicStripFull.Periodical as Periodical
import qualified StarTrek.ComicStripFull.PublishedDayFrom as PublishedDayFrom
import qualified StarTrek.ComicStripFull.PublishedDayTo as PublishedDayTo
import qualified StarTrek.ComicStripFull.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.ComicStripFull.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.ComicStripFull.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.ComicStripFull.PublishedYearTo as PublishedYearTo
import qualified StarTrek.ComicStripFull.Title as Title
import qualified StarTrek.ComicStripFull.Uid as Uid
import qualified StarTrek.ComicStripFull.YearFrom as YearFrom
import qualified StarTrek.ComicStripFull.YearTo as YearTo
import qualified StarTrek.StaffBase as StaffBase

data ComicStripFull = ComicStripFull
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic strip stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic strip was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic strip was published
  , uid :: Uid.Uid -- ^ Comic strip unique ID
  , artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic strip was published
  , title :: Title.Title -- ^ Comic strip title
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic strip was published
  , comicSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic strip stories
  , periodical :: Maybe Periodical.Periodical -- ^ Title of the periodical the comic strip was published in
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic strip was published
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic strip was published
  }
  deriving (Eq, Show)

comicStripFullSchema :: FC.Fleece schema => schema ComicStripFull
comicStripFullSchema =
  FC.object $
    FC.constructor ComicStripFull
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "comicSeries" comicSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "periodical" periodical Periodical.periodicalSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema