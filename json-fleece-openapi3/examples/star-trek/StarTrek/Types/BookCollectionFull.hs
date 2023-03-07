{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionFull
  ( BookCollectionFull(..)
  , bookCollectionFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookBase as BookBase
import qualified StarTrek.Types.BookCollectionFull.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.BookCollectionFull.PublishedDay as PublishedDay
import qualified StarTrek.Types.BookCollectionFull.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.BookCollectionFull.PublishedYear as PublishedYear
import qualified StarTrek.Types.BookCollectionFull.StardateFrom as StardateFrom
import qualified StarTrek.Types.BookCollectionFull.StardateTo as StardateTo
import qualified StarTrek.Types.BookCollectionFull.Title as Title
import qualified StarTrek.Types.BookCollectionFull.Uid as Uid
import qualified StarTrek.Types.BookCollectionFull.YearFrom as YearFrom
import qualified StarTrek.Types.BookCollectionFull.YearTo as YearTo
import qualified StarTrek.Types.BookSeriesBase as BookSeriesBase
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.Reference as Reference
import qualified StarTrek.Types.StaffBase as StaffBase

data BookCollectionFull = BookCollectionFull
  { authors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book collection stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of book collection stories
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , bookSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the book collection was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the book collection was published
  , books :: Maybe [BookBase.BookBase] -- ^ Base book, returned in search results
  , uid :: Maybe Uid.Uid -- ^ Book collection unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of book collection stories
  , artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the book collection was published
  , title :: Maybe Title.Title -- ^ Book collection title
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book collection stories
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

bookCollectionFullSchema :: FC.Fleece schema => schema BookCollectionFull
bookCollectionFullSchema =
  FC.object $
    FC.constructor BookCollectionFull
      #+ FC.optional "authors" authors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "bookSeries" bookSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "books" books (FC.list BookBase.bookBaseSchema)
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "title" title Title.titleSchema
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)