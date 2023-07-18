{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsFull
  ( ComicsFull(..)
  , comicsFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.ComicCollectionBase as ComicCollectionBase
import qualified StarTrek.Types.ComicSeriesBase as ComicSeriesBase
import qualified StarTrek.Types.ComicsFull.Adaptation as Adaptation
import qualified StarTrek.Types.ComicsFull.CoverDay as CoverDay
import qualified StarTrek.Types.ComicsFull.CoverMonth as CoverMonth
import qualified StarTrek.Types.ComicsFull.CoverYear as CoverYear
import qualified StarTrek.Types.ComicsFull.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.ComicsFull.Photonovel as Photonovel
import qualified StarTrek.Types.ComicsFull.PublishedDay as PublishedDay
import qualified StarTrek.Types.ComicsFull.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.ComicsFull.PublishedYear as PublishedYear
import qualified StarTrek.Types.ComicsFull.StardateFrom as StardateFrom
import qualified StarTrek.Types.ComicsFull.StardateTo as StardateTo
import qualified StarTrek.Types.ComicsFull.Title as Title
import qualified StarTrek.Types.ComicsFull.Uid as Uid
import qualified StarTrek.Types.ComicsFull.YearFrom as YearFrom
import qualified StarTrek.Types.ComicsFull.YearTo as YearTo
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.Reference as Reference
import qualified StarTrek.Types.StaffBase as StaffBase

data ComicsFull = ComicsFull
  { coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic story
  , comicSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic story
  , uid :: Uid.Uid -- ^ Comics unique ID
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the comics was published
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the comics was published
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic story
  , title :: Title.Title -- ^ Comics title
  , artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , comicCollections :: Maybe [ComicCollectionBase.ComicCollectionBase] -- ^ Base comic collection, returned in search results
  , adaptation :: Maybe Adaptation.Adaptation -- ^ Whether it's an adaptation of an episode or a movie
  , photonovel :: Maybe Photonovel.Photonovel -- ^ Whether it's a photonovel
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic  story
  , staff :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the comics was published
  }
  deriving (Eq, Show)

comicsFullSchema :: FC.Fleece schema => schema ComicsFull
comicsFullSchema =
  FC.object $
    FC.constructor ComicsFull
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "comicSeries" comicSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "comicCollections" comicCollections (FC.list ComicCollectionBase.comicCollectionBaseSchema)
      #+ FC.optional "adaptation" adaptation Adaptation.adaptationSchema
      #+ FC.optional "photonovel" photonovel Photonovel.photonovelSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "staff" staff (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema