{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsFull
  ( ComicsFull(..)
  , comicsFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ComicCollectionBase (ComicCollectionBase, comicCollectionBaseSchema)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicsFull.Adaptation (Adaptation, adaptationSchema)
import StarTrek.ComicsFull.CoverDay (CoverDay, coverDaySchema)
import StarTrek.ComicsFull.CoverMonth (CoverMonth, coverMonthSchema)
import StarTrek.ComicsFull.CoverYear (CoverYear, coverYearSchema)
import StarTrek.ComicsFull.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.ComicsFull.Photonovel (Photonovel, photonovelSchema)
import StarTrek.ComicsFull.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.ComicsFull.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.ComicsFull.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.ComicsFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.ComicsFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.ComicsFull.Title (Title, titleSchema)
import StarTrek.ComicsFull.Uid (Uid, uidSchema)
import StarTrek.ComicsFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicsFull.YearTo (YearTo, yearToSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data ComicsFull = ComicsFull
  { comicCollections :: Maybe [ComicCollectionBase] -- ^ Base comic collection, returned in search results
  , yearFrom :: Maybe YearFrom -- ^ Starting year of comic  story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of comic story
  , adaptation :: Maybe Adaptation -- ^ Whether it's an adaptation of an episode or a movie
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the comics was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the comics was published
  , uid :: Uid -- ^ Comics unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of comic story
  , artists :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , publishedDay :: Maybe PublishedDay -- ^ Day the comics was published
  , photonovel :: Maybe Photonovel -- ^ Whether it's a photonovel
  , coverYear :: Maybe CoverYear -- ^ Cover publication year
  , title :: Title -- ^ Comics title
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ Base comic series, returned in search results
  , coverDay :: Maybe CoverDay -- ^ Cover publication day
  , references :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo -- ^ Ending year of comic story
  , staff :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , writers :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , coverMonth :: Maybe CoverMonth -- ^ Cover publication month
  , editors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

comicsFullSchema :: FC.Fleece schema => schema ComicsFull
comicsFullSchema =
  FC.object $
    FC.constructor ComicsFull
      #+ FC.optional "comicCollections" comicCollections (FC.list comicCollectionBaseSchema)
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "adaptation" adaptation adaptationSchema
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
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.optional "writers" writers (FC.list staffBaseSchema)
      #+ FC.optional "coverMonth" coverMonth coverMonthSchema
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)