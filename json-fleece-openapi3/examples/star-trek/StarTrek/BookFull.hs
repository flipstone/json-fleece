{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull
  ( BookFull(..)
  , bookFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookCollectionBase (BookCollectionBase, bookCollectionBaseSchema)
import StarTrek.BookFull.Anthology (Anthology, anthologySchema)
import StarTrek.BookFull.Audiobook (Audiobook, audiobookSchema)
import StarTrek.BookFull.AudiobookAbridged (AudiobookAbridged, audiobookAbridgedSchema)
import StarTrek.BookFull.AudiobookPublishedDay (AudiobookPublishedDay, audiobookPublishedDaySchema)
import StarTrek.BookFull.AudiobookPublishedMonth (AudiobookPublishedMonth, audiobookPublishedMonthSchema)
import StarTrek.BookFull.AudiobookPublishedYear (AudiobookPublishedYear, audiobookPublishedYearSchema)
import StarTrek.BookFull.AudiobookRunTime (AudiobookRunTime, audiobookRunTimeSchema)
import StarTrek.BookFull.BiographyBook (BiographyBook, biographyBookSchema)
import StarTrek.BookFull.EBook (EBook, eBookSchema)
import StarTrek.BookFull.Novel (Novel, novelSchema)
import StarTrek.BookFull.Novelization (Novelization, novelizationSchema)
import StarTrek.BookFull.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.BookFull.ProductionNumber (ProductionNumber, productionNumberSchema)
import StarTrek.BookFull.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.BookFull.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.BookFull.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.BookFull.ReferenceBook (ReferenceBook, referenceBookSchema)
import StarTrek.BookFull.RolePlayingBook (RolePlayingBook, rolePlayingBookSchema)
import StarTrek.BookFull.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.BookFull.StardateTo (StardateTo, stardateToSchema)
import StarTrek.BookFull.Title (Title, titleSchema)
import StarTrek.BookFull.Uid (Uid, uidSchema)
import StarTrek.BookFull.YearFrom (YearFrom, yearFromSchema)
import StarTrek.BookFull.YearTo (YearTo, yearToSchema)
import StarTrek.BookSeriesBase (BookSeriesBase, bookSeriesBaseSchema)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data BookFull = BookFull
  { anthology :: Anthology -- ^ Whether it's an anthology
  , audiobookPublishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , authors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom -- ^ Starting year of book story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of book story
  , audiobookNarrators :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , audiobookAbridged :: AudiobookAbridged -- ^ If it's an audiobook, whether it's been abridged
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , bookSeries :: Maybe [BookSeriesBase] -- ^ Base book series, returned in search results
  , audiobookPublishedDay :: Maybe AudiobookPublishedDay -- ^ Day the audiobook was published
  , productionNumber :: Maybe ProductionNumber -- ^ Book production number
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the book was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the book was published
  , uid :: Uid -- ^ Book unique ID
  , bookCollections :: Maybe [BookCollectionBase] -- ^ Base book collection, returned in search results
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of book story
  , artists :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , publishedDay :: Maybe PublishedDay -- ^ Day the book was published
  , novel :: Novel -- ^ Whether it's a novel
  , audiobookRunTime :: Maybe AudiobookRunTime -- ^ Audiobook run time, in minutes
  , title :: Title -- ^ Book title
  , referenceBook :: ReferenceBook -- ^ Whether it's a reference book
  , references :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , audiobookPublishedMonth :: Maybe AudiobookPublishedMonth -- ^ Month the audiobook was published
  , yearTo :: Maybe YearTo -- ^ Ending year of book story
  , audiobookReferences :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , audiobookPublishedYear :: Maybe AudiobookPublishedYear -- ^ Year the audiobook was published
  , biographyBook :: BiographyBook -- ^ Whether it's a biography book
  , rolePlayingBook :: RolePlayingBook -- ^ Whether it's a role playing book
  , novelization :: Novelization -- ^ Whether it's a novelization
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , eBook :: EBook -- ^ Whether it's an e-book
  , editors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , audiobook :: Audiobook -- ^ Whether it's an audiobook, or has been release as an audiobook in addition to other form
  }
  deriving (Eq, Show)

bookFullSchema :: FC.Fleece schema => schema BookFull
bookFullSchema =
  FC.object $
    FC.constructor BookFull
      #+ FC.required "anthology" anthology anthologySchema
      #+ FC.optional "audiobookPublishers" audiobookPublishers (FC.list companyBaseSchema)
      #+ FC.optional "authors" authors (FC.list staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "audiobookNarrators" audiobookNarrators (FC.list staffBaseSchema)
      #+ FC.required "audiobookAbridged" audiobookAbridged audiobookAbridgedSchema
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "bookSeries" bookSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optional "audiobookPublishedDay" audiobookPublishedDay audiobookPublishedDaySchema
      #+ FC.optional "productionNumber" productionNumber productionNumberSchema
      #+ FC.optional "publishedMonth" publishedMonth publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear publishedYearSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "bookCollections" bookCollections (FC.list bookCollectionBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "artists" artists (FC.list staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay publishedDaySchema
      #+ FC.required "novel" novel novelSchema
      #+ FC.optional "audiobookRunTime" audiobookRunTime audiobookRunTimeSchema
      #+ FC.required "title" title titleSchema
      #+ FC.required "referenceBook" referenceBook referenceBookSchema
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "audiobookPublishedMonth" audiobookPublishedMonth audiobookPublishedMonthSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "audiobookReferences" audiobookReferences (FC.list referenceSchema)
      #+ FC.optional "audiobookPublishedYear" audiobookPublishedYear audiobookPublishedYearSchema
      #+ FC.required "biographyBook" biographyBook biographyBookSchema
      #+ FC.required "rolePlayingBook" rolePlayingBook rolePlayingBookSchema
      #+ FC.required "novelization" novelization novelizationSchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.required "eBook" eBook eBookSchema
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)
      #+ FC.required "audiobook" audiobook audiobookSchema