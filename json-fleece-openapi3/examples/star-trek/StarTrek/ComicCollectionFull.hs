{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionFull
  ( ComicCollectionFull(..)
  , comicCollectionFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicCollectionFull.CoverDay (CoverDay, coverDaySchema)
import StarTrek.ComicCollectionFull.CoverMonth (CoverMonth, coverMonthSchema)
import StarTrek.ComicCollectionFull.CoverYear (CoverYear, coverYearSchema)
import StarTrek.ComicCollectionFull.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.ComicCollectionFull.Photonovel (Photonovel, photonovelSchema)
import StarTrek.ComicCollectionFull.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.ComicCollectionFull.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.ComicCollectionFull.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.ComicCollectionFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.ComicCollectionFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.ComicCollectionFull.Title (Title, titleSchema)
import StarTrek.ComicCollectionFull.Uid (Uid, uidSchema)
import StarTrek.ComicCollectionFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicCollectionFull.YearTo (YearTo, yearToSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicsBase (ComicsBase, comicsBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicCollectionFull = ComicCollectionFull
  { yearFrom :: Maybe YearFrom -- ^ Starting year of comic collection stories
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of comic collection stories
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the comic collection was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the comic collection was published
  , uid :: Uid -- ^ Comic collection unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of comic collection stories
  , artists :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , publishedDay :: Maybe PublishedDay -- ^ Day the comic collection was published
  , photonovel :: Maybe Photonovel -- ^ Whether it's a photonovel collection
  , coverYear :: Maybe CoverYear -- ^ Cover publication year
  , title :: Title -- ^ Comic collection title
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ Base comic series, returned in search results
  , coverDay :: Maybe CoverDay -- ^ Cover publication day
  , references :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo -- ^ Ending year of comic collection stories
  , staff :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , comics :: Maybe [ComicsBase] -- ^ Base comics, returned in search results
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , writers :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , coverMonth :: Maybe CoverMonth -- ^ Cover publication month
  , editors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

comicCollectionFullSchema :: FC.Fleece schema => schema ComicCollectionFull
comicCollectionFullSchema =
  FC.object $
    FC.constructor ComicCollectionFull
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "publishedMonth" publishedMonth publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear publishedYearSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "artists" artists (FC.list staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay publishedDaySchema
      #+ FC.optional "photonovel" photonovel photonovelSchema
      #+ FC.optional "coverYear" coverYear coverYearSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optional "coverDay" coverDay coverDaySchema
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "staff" staff (FC.list staffBaseSchema)
      #+ FC.optional "comics" comics (FC.list comicsBaseSchema)
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "coverMonth" coverMonth coverMonthSchema
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)