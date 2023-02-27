{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionFull
  ( BookCollectionFull(..)
  , bookCollectionFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookBase (BookBase, bookBaseSchema)
import StarTrek.BookCollectionFull.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.BookCollectionFull.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.BookCollectionFull.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.BookCollectionFull.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.BookCollectionFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.BookCollectionFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.BookCollectionFull.Title (Title, titleSchema)
import StarTrek.BookCollectionFull.Uid (Uid, uidSchema)
import StarTrek.BookCollectionFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.BookCollectionFull.YearTo (YearTo, yearToSchema)
import StarTrek.BookSeriesBase (BookSeriesBase, bookSeriesBaseSchema)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data BookCollectionFull = BookCollectionFull
  { authors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom -- ^ Starting year of book collection stories
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of book collection stories
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , bookSeries :: Maybe [BookSeriesBase] -- ^ Base book series, returned in search results
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the book collection was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the book collection was published
  , books :: Maybe [BookBase] -- ^ Base book, returned in search results
  , uid :: Maybe Uid -- ^ Book collection unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of book collection stories
  , artists :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , publishedDay :: Maybe PublishedDay -- ^ Day the book collection was published
  , title :: Maybe Title -- ^ Book collection title
  , references :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , yearTo :: Maybe YearTo -- ^ Ending year of book collection stories
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , editors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

bookCollectionFullSchema :: FC.Fleece schema => schema BookCollectionFull
bookCollectionFullSchema =
  FC.object $
    FC.constructor BookCollectionFull
      #+ FC.optional "authors" authors (FC.list staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "bookSeries" bookSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optional "publishedMonth" publishedMonth publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear publishedYearSchema
      #+ FC.optional "books" books (FC.list bookBaseSchema)
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "artists" artists (FC.list staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay publishedDaySchema
      #+ FC.optional "title" title titleSchema
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)