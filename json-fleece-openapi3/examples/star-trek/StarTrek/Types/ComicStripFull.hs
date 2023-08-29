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
  { title :: Title.Title -- ^ Comic strip title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic strip stories
  , periodical :: Maybe Periodical.Periodical -- ^ Title of the periodical the comic strip was published in
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the comic strip was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the comic strip was published
  , publishedDayFrom :: Maybe PublishedDayFrom.PublishedDayFrom -- ^ Day from which the comic strip was published
  , artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , comicSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , uid :: Uid.Uid -- ^ Comic strip unique ID
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the comic strip was published
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic strip stories
  , publishedDayTo :: Maybe PublishedDayTo.PublishedDayTo -- ^ Day to which the comic strip was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the comic strip was published
  }
  deriving (Eq, Show)

comicStripFullSchema :: FC.Fleece schema => schema ComicStripFull
comicStripFullSchema =
  FC.object $
    FC.constructor ComicStripFull
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "periodical" periodical Periodical.periodicalSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom PublishedDayFrom.publishedDayFromSchema
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "comicSeries" comicSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "publishedDayTo" publishedDayTo PublishedDayTo.publishedDayToSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema