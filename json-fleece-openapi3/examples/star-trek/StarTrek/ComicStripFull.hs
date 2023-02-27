{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripFull
  ( ComicStripFull(..)
  , comicStripFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicStripFull.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.ComicStripFull.Periodical (Periodical, periodicalSchema)
import StarTrek.ComicStripFull.PublishedDayFrom (PublishedDayFrom, publishedDayFromSchema)
import StarTrek.ComicStripFull.PublishedDayTo (PublishedDayTo, publishedDayToSchema)
import StarTrek.ComicStripFull.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.ComicStripFull.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.ComicStripFull.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.ComicStripFull.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.ComicStripFull.Title (Title, titleSchema)
import StarTrek.ComicStripFull.Uid (Uid, uidSchema)
import StarTrek.ComicStripFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicStripFull.YearTo (YearTo, yearToSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicStripFull = ComicStripFull
  { yearFrom :: Maybe YearFrom -- ^ Starting year of comic strip stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the comic strip was published
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the comic strip was published
  , uid :: Uid -- ^ Comic strip unique ID
  , artists :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the comic strip was published
  , title :: Title -- ^ Comic strip title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the comic strip was published
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ Base comic series, returned in search results
  , yearTo :: Maybe YearTo -- ^ Ending year of comic strip stories
  , periodical :: Maybe Periodical -- ^ Title of the periodical the comic strip was published in
  , publishedDayTo :: Maybe PublishedDayTo -- ^ Day to which the comic strip was published
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , writers :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , publishedDayFrom :: Maybe PublishedDayFrom -- ^ Day from which the comic strip was published
  }
  deriving (Eq, Show)

comicStripFullSchema :: FC.Fleece schema => schema ComicStripFull
comicStripFullSchema =
  FC.object $
    FC.constructor ComicStripFull
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "artists" artists (FC.list staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema
      #+ FC.optional "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "periodical" periodical periodicalSchema
      #+ FC.optional "publishedDayTo" publishedDayTo publishedDayToSchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "publishedDayFrom" publishedDayFrom publishedDayFromSchema