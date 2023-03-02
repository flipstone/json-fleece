{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull
  ( BookFull(..)
  , bookFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookCollectionBase as BookCollectionBase
import qualified StarTrek.BookFull.Anthology as Anthology
import qualified StarTrek.BookFull.Audiobook as Audiobook
import qualified StarTrek.BookFull.AudiobookAbridged as AudiobookAbridged
import qualified StarTrek.BookFull.AudiobookPublishedDay as AudiobookPublishedDay
import qualified StarTrek.BookFull.AudiobookPublishedMonth as AudiobookPublishedMonth
import qualified StarTrek.BookFull.AudiobookPublishedYear as AudiobookPublishedYear
import qualified StarTrek.BookFull.AudiobookRunTime as AudiobookRunTime
import qualified StarTrek.BookFull.BiographyBook as BiographyBook
import qualified StarTrek.BookFull.EBook as EBook
import qualified StarTrek.BookFull.Novel as Novel
import qualified StarTrek.BookFull.Novelization as Novelization
import qualified StarTrek.BookFull.NumberOfPages as NumberOfPages
import qualified StarTrek.BookFull.ProductionNumber as ProductionNumber
import qualified StarTrek.BookFull.PublishedDay as PublishedDay
import qualified StarTrek.BookFull.PublishedMonth as PublishedMonth
import qualified StarTrek.BookFull.PublishedYear as PublishedYear
import qualified StarTrek.BookFull.ReferenceBook as ReferenceBook
import qualified StarTrek.BookFull.RolePlayingBook as RolePlayingBook
import qualified StarTrek.BookFull.StardateFrom as StardateFrom
import qualified StarTrek.BookFull.StardateTo as StardateTo
import qualified StarTrek.BookFull.Title as Title
import qualified StarTrek.BookFull.Uid as Uid
import qualified StarTrek.BookFull.YearFrom as YearFrom
import qualified StarTrek.BookFull.YearTo as YearTo
import qualified StarTrek.BookSeriesBase as BookSeriesBase
import qualified StarTrek.CharacterBase as CharacterBase
import qualified StarTrek.CompanyBase as CompanyBase
import qualified StarTrek.Reference as Reference
import qualified StarTrek.StaffBase as StaffBase

data BookFull = BookFull
  { anthology :: Anthology.Anthology -- ^ Whether it's an anthology
  , audiobookPublishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , authors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of book story
  , audiobookNarrators :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , audiobookAbridged :: AudiobookAbridged.AudiobookAbridged -- ^ If it's an audiobook, whether it's been abridged
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , bookSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , audiobookPublishedDay :: Maybe AudiobookPublishedDay.AudiobookPublishedDay -- ^ Day the audiobook was published
  , productionNumber :: Maybe ProductionNumber.ProductionNumber -- ^ Book production number
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the book was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the book was published
  , uid :: Uid.Uid -- ^ Book unique ID
  , bookCollections :: Maybe [BookCollectionBase.BookCollectionBase] -- ^ Base book collection, returned in search results
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of book story
  , artists :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the book was published
  , novel :: Novel.Novel -- ^ Whether it's a novel
  , audiobookRunTime :: Maybe AudiobookRunTime.AudiobookRunTime -- ^ Audiobook run time, in minutes
  , title :: Title.Title -- ^ Book title
  , referenceBook :: ReferenceBook.ReferenceBook -- ^ Whether it's a reference book
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , audiobookPublishedMonth :: Maybe AudiobookPublishedMonth.AudiobookPublishedMonth -- ^ Month the audiobook was published
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book story
  , audiobookReferences :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , audiobookPublishedYear :: Maybe AudiobookPublishedYear.AudiobookPublishedYear -- ^ Year the audiobook was published
  , biographyBook :: BiographyBook.BiographyBook -- ^ Whether it's a biography book
  , rolePlayingBook :: RolePlayingBook.RolePlayingBook -- ^ Whether it's a role playing book
  , novelization :: Novelization.Novelization -- ^ Whether it's a novelization
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , eBook :: EBook.EBook -- ^ Whether it's an e-book
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , audiobook :: Audiobook.Audiobook -- ^ Whether it's an audiobook, or has been release as an audiobook in addition to other form
  }
  deriving (Eq, Show)

bookFullSchema :: FC.Fleece schema => schema BookFull
bookFullSchema =
  FC.object $
    FC.constructor BookFull
      #+ FC.required "anthology" anthology Anthology.anthologySchema
      #+ FC.optional "audiobookPublishers" audiobookPublishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "authors" authors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "audiobookNarrators" audiobookNarrators (FC.list StaffBase.staffBaseSchema)
      #+ FC.required "audiobookAbridged" audiobookAbridged AudiobookAbridged.audiobookAbridgedSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "bookSeries" bookSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.optional "audiobookPublishedDay" audiobookPublishedDay AudiobookPublishedDay.audiobookPublishedDaySchema
      #+ FC.optional "productionNumber" productionNumber ProductionNumber.productionNumberSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "bookCollections" bookCollections (FC.list BookCollectionBase.bookCollectionBaseSchema)
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "artists" artists (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.required "novel" novel Novel.novelSchema
      #+ FC.optional "audiobookRunTime" audiobookRunTime AudiobookRunTime.audiobookRunTimeSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "referenceBook" referenceBook ReferenceBook.referenceBookSchema
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "audiobookPublishedMonth" audiobookPublishedMonth AudiobookPublishedMonth.audiobookPublishedMonthSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "audiobookReferences" audiobookReferences (FC.list Reference.referenceSchema)
      #+ FC.optional "audiobookPublishedYear" audiobookPublishedYear AudiobookPublishedYear.audiobookPublishedYearSchema
      #+ FC.required "biographyBook" biographyBook BiographyBook.biographyBookSchema
      #+ FC.required "rolePlayingBook" rolePlayingBook RolePlayingBook.rolePlayingBookSchema
      #+ FC.required "novelization" novelization Novelization.novelizationSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.required "eBook" eBook EBook.eBookSchema
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)
      #+ FC.required "audiobook" audiobook Audiobook.audiobookSchema