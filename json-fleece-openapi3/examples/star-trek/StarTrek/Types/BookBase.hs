{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase
  ( BookBase(..)
  , bookBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookBase.Anthology as Anthology
import qualified StarTrek.Types.BookBase.Audiobook as Audiobook
import qualified StarTrek.Types.BookBase.AudiobookAbridged as AudiobookAbridged
import qualified StarTrek.Types.BookBase.AudiobookPublishedDay as AudiobookPublishedDay
import qualified StarTrek.Types.BookBase.AudiobookPublishedMonth as AudiobookPublishedMonth
import qualified StarTrek.Types.BookBase.AudiobookPublishedYear as AudiobookPublishedYear
import qualified StarTrek.Types.BookBase.AudiobookRunTime as AudiobookRunTime
import qualified StarTrek.Types.BookBase.BiographyBook as BiographyBook
import qualified StarTrek.Types.BookBase.EBook as EBook
import qualified StarTrek.Types.BookBase.Novel as Novel
import qualified StarTrek.Types.BookBase.Novelization as Novelization
import qualified StarTrek.Types.BookBase.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.BookBase.ProductionNumber as ProductionNumber
import qualified StarTrek.Types.BookBase.PublishedDay as PublishedDay
import qualified StarTrek.Types.BookBase.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.BookBase.PublishedYear as PublishedYear
import qualified StarTrek.Types.BookBase.ReferenceBook as ReferenceBook
import qualified StarTrek.Types.BookBase.RolePlayingBook as RolePlayingBook
import qualified StarTrek.Types.BookBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.BookBase.StardateTo as StardateTo
import qualified StarTrek.Types.BookBase.Title as Title
import qualified StarTrek.Types.BookBase.Uid as Uid
import qualified StarTrek.Types.BookBase.YearFrom as YearFrom
import qualified StarTrek.Types.BookBase.YearTo as YearTo

data BookBase = BookBase
  { anthology :: Anthology.Anthology -- ^ Whether it's an anthology
  , audiobook :: Audiobook.Audiobook -- ^ Whether it's an audiobook, or has been release as an audiobook in addition to other form
  , audiobookAbridged :: AudiobookAbridged.AudiobookAbridged -- ^ If it's an audiobook, whether it's been abridged
  , audiobookPublishedDay :: Maybe AudiobookPublishedDay.AudiobookPublishedDay -- ^ Day the audiobook was published
  , audiobookPublishedMonth :: Maybe AudiobookPublishedMonth.AudiobookPublishedMonth -- ^ Month the audiobook was published
  , audiobookPublishedYear :: Maybe AudiobookPublishedYear.AudiobookPublishedYear -- ^ Year the audiobook was published
  , audiobookRunTime :: Maybe AudiobookRunTime.AudiobookRunTime -- ^ Audiobook run time, in minutes
  , biographyBook :: BiographyBook.BiographyBook -- ^ Whether it's a biography book
  , eBook :: EBook.EBook -- ^ Whether it's an eBook
  , novel :: Novel.Novel -- ^ Whether it's a novel
  , novelization :: Novelization.Novelization -- ^ Whether it's a novelization
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , productionNumber :: Maybe ProductionNumber.ProductionNumber -- ^ Book's production number
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the book was published
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the book was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the book was published
  , referenceBook :: ReferenceBook.ReferenceBook -- ^ Whether it's a reference book
  , rolePlayingBook :: RolePlayingBook.RolePlayingBook -- ^ Whether it's a role playing book
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of book story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of book story
  , title :: Title.Title -- ^ Book title
  , uid :: Uid.Uid -- ^ Book unique ID
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book story
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book story
  }
  deriving (Eq, Show)

bookBaseSchema :: FC.Fleece t => FC.Schema t BookBase
bookBaseSchema =
  FC.object $
    FC.constructor BookBase
      #+ FC.required "anthology" anthology Anthology.anthologySchema
      #+ FC.required "audiobook" audiobook Audiobook.audiobookSchema
      #+ FC.required "audiobookAbridged" audiobookAbridged AudiobookAbridged.audiobookAbridgedSchema
      #+ FC.optional "audiobookPublishedDay" audiobookPublishedDay AudiobookPublishedDay.audiobookPublishedDaySchema
      #+ FC.optional "audiobookPublishedMonth" audiobookPublishedMonth AudiobookPublishedMonth.audiobookPublishedMonthSchema
      #+ FC.optional "audiobookPublishedYear" audiobookPublishedYear AudiobookPublishedYear.audiobookPublishedYearSchema
      #+ FC.optional "audiobookRunTime" audiobookRunTime AudiobookRunTime.audiobookRunTimeSchema
      #+ FC.required "biographyBook" biographyBook BiographyBook.biographyBookSchema
      #+ FC.required "eBook" eBook EBook.eBookSchema
      #+ FC.required "novel" novel Novel.novelSchema
      #+ FC.required "novelization" novelization Novelization.novelizationSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "productionNumber" productionNumber ProductionNumber.productionNumberSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.required "referenceBook" referenceBook ReferenceBook.referenceBookSchema
      #+ FC.required "rolePlayingBook" rolePlayingBook RolePlayingBook.rolePlayingBookSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema