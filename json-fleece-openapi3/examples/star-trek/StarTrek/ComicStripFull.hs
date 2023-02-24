{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripFull
  ( ComicStripFull(..)
  , comicStripFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "artists" artists (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "periodical" periodical FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDayTo" publishedDayTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "writers" writers (FC.list staffBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDayFrom" publishedDayFrom FC.integer