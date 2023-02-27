{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripFull
  ( ComicStripFull(..)
  , comicStripFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicStripFull = ComicStripFull
  { yearFrom :: Maybe Integer -- ^ Starting year of comic strip stories
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the comic strip was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the comic strip was published
  , uid :: Text -- ^ Comic strip unique ID
  , artists :: Maybe [StaffBase] -- ^ Artists involved in the comic strip
  , characters :: Maybe [CharacterBase] -- ^ Characters appearing in the comic strip
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the comic strip was published
  , title :: Text -- ^ Comic strip title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the comic strip was published
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ Comic series this comic strip is included in
  , yearTo :: Maybe Integer -- ^ Ending year of comic strip stories
  , periodical :: Maybe Text -- ^ Title of the periodical the comic strip was published in
  , publishedDayTo :: Maybe Integer -- ^ Day to which the comic strip was published
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , writers :: Maybe [StaffBase] -- ^ Writers involved in the comic strip
  , publishedDayFrom :: Maybe Integer -- ^ Day from which the comic strip was published
  }
  deriving (Eq, Show)

comicStripFullSchema :: FC.Fleece schema => schema ComicStripFull
comicStripFullSchema =
  FC.object $
    FC.constructor ComicStripFull
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "artists" artists (FC.list staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optional "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "periodical" periodical FC.text
      #+ FC.optional "publishedDayTo" publishedDayTo FC.integer
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "publishedDayFrom" publishedDayFrom FC.integer