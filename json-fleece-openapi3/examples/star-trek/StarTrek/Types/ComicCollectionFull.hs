{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicCollectionFull
  ( ComicCollectionFull(..)
  , comicCollectionFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.ComicCollectionFull.CoverDay as CoverDay
import qualified StarTrek.Types.ComicCollectionFull.CoverMonth as CoverMonth
import qualified StarTrek.Types.ComicCollectionFull.CoverYear as CoverYear
import qualified StarTrek.Types.ComicCollectionFull.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.ComicCollectionFull.Photonovel as Photonovel
import qualified StarTrek.Types.ComicCollectionFull.PublishedDay as PublishedDay
import qualified StarTrek.Types.ComicCollectionFull.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.ComicCollectionFull.PublishedYear as PublishedYear
import qualified StarTrek.Types.ComicCollectionFull.StardateFrom as StardateFrom
import qualified StarTrek.Types.ComicCollectionFull.StardateTo as StardateTo
import qualified StarTrek.Types.ComicCollectionFull.Title as Title
import qualified StarTrek.Types.ComicCollectionFull.Uid as Uid
import qualified StarTrek.Types.ComicCollectionFull.YearFrom as YearFrom
import qualified StarTrek.Types.ComicCollectionFull.YearTo as YearTo
import qualified StarTrek.Types.ComicSeriesBase as ComicSeriesBase
import qualified StarTrek.Types.ComicsBase as ComicsBase
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.Reference as Reference
import qualified StarTrek.Types.StaffBase as StaffBase

data ComicCollectionFull = ComicCollectionFull
  { artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , comicSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , comics :: Maybe [ComicsBase.ComicsBase] -- ^ Base comics, returned in search results
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , photonovel :: Maybe Photonovel.Photonovel -- ^ Whether it's a photonovel collection
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the comic collection was published
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the comic collection was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the comic collection was published
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , staff :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic collection stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic collection stories
  , title :: Title.Title -- ^ Comic collection title
  , uid :: Uid.Uid -- ^ Comic collection unique ID
  , writers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic collection stories
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic collection stories
  }
  deriving (Eq, Show)

comicCollectionFullSchema :: FC.Fleece schema => schema ComicCollectionFull
comicCollectionFullSchema =
  FC.object $
    FC.constructor ComicCollectionFull
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "comicSeries" comicSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "comics" comics (FC.list ComicsBase.comicsBaseSchema)
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "photonovel" photonovel Photonovel.photonovelSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "staff" staff (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "writers" writers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema