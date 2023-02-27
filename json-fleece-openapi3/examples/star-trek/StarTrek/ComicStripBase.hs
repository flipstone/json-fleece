{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripBase
  ( ComicStripBase(..)
  , comicStripBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicStripBase.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.ComicStripBase.Periodical (Periodical, periodicalSchema)
import StarTrek.ComicStripBase.PublishedDayFrom (PublishedDayFrom, publishedDayFromSchema)
import StarTrek.ComicStripBase.PublishedDayTo (PublishedDayTo, publishedDayToSchema)
import StarTrek.ComicStripBase.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.ComicStripBase.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.ComicStripBase.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.ComicStripBase.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.ComicStripBase.Title (Title, titleSchema)
import StarTrek.ComicStripBase.Uid (Uid, uidSchema)
import StarTrek.ComicStripBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicStripBase.YearTo (YearTo, yearToSchema)

data ComicStripBase = ComicStripBase
  { yearFrom :: Maybe YearFrom -- ^ Starting year of comic strip story
  , publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the comic strip was published
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the comic strip was published
  , uid :: Uid -- ^ Comic strip unique ID
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the comic strip was published
  , title :: Title -- ^ Comic strip title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the comic strip was published
  , yearTo :: Maybe YearTo -- ^ Ending year of comic strip story
  , periodical :: Maybe Periodical -- ^ Title of the periodical the comic strip was published in
  , publishedDayTo :: Maybe PublishedDayTo -- ^ Day to which the comic strip was published
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , publishedDayFrom :: Maybe PublishedDayFrom -- ^ Day from which the comic strip was published
  }
  deriving (Eq, Show)

comicStripBaseSchema :: FC.Fleece schema => schema ComicStripBase
comicStripBaseSchema =
  FC.object $
    FC.constructor ComicStripBase
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "periodical" periodical periodicalSchema
      #+ FC.optional "publishedDayTo" publishedDayTo publishedDayToSchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom publishedDayFromSchema