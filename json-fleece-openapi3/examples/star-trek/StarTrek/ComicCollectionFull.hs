{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionFull
  ( ComicCollectionFull(..)
  , comicCollectionFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CharacterBase as CharacterBase
import qualified StarTrek.ComicCollectionFull.CoverDay as CoverDay
import qualified StarTrek.ComicCollectionFull.CoverMonth as CoverMonth
import qualified StarTrek.ComicCollectionFull.CoverYear as CoverYear
import qualified StarTrek.ComicCollectionFull.NumberOfPages as NumberOfPages
import qualified StarTrek.ComicCollectionFull.Photonovel as Photonovel
import qualified StarTrek.ComicCollectionFull.PublishedDay as PublishedDay
import qualified StarTrek.ComicCollectionFull.PublishedMonth as PublishedMonth
import qualified StarTrek.ComicCollectionFull.PublishedYear as PublishedYear
import qualified StarTrek.ComicCollectionFull.StardateFrom as StardateFrom
import qualified StarTrek.ComicCollectionFull.StardateTo as StardateTo
import qualified StarTrek.ComicCollectionFull.Title as Title
import qualified StarTrek.ComicCollectionFull.Uid as Uid
import qualified StarTrek.ComicCollectionFull.YearFrom as YearFrom
import qualified StarTrek.ComicCollectionFull.YearTo as YearTo
import qualified StarTrek.ComicSeriesBase as ComicSeriesBase
import qualified StarTrek.ComicsBase as ComicsBase
import qualified StarTrek.CompanyBase as CompanyBase
import qualified StarTrek.Reference as Reference
import qualified StarTrek.StaffBase as StaffBase

data ComicCollectionFull = ComicCollectionFull
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic collection stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic collection stories
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the comic collection was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the comic collection was published
  , uid :: Uid.Uid -- ^ Comic collection unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic collection stories
  , artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the comic collection was published
  , photonovel :: Maybe Photonovel.Photonovel -- ^ Whether it's a photonovel collection
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , title :: Title.Title -- ^ Comic collection title
  , comicSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic collection stories
  , staff :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , comics :: Maybe [ComicsBase.ComicsBase] -- ^ Base comics, returned in search results
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

comicCollectionFullSchema :: FC.Fleece schema => schema ComicCollectionFull
comicCollectionFullSchema =
  FC.object $
    FC.constructor ComicCollectionFull
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "photonovel" photonovel Photonovel.photonovelSchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "comicSeries" comicSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "staff" staff (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "comics" comics (FC.list ComicsBase.comicsBaseSchema)
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)