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
  { biographyBook :: BiographyBook.BiographyBook -- ^ Whether it's a biography book
  , audiobookPublishedDay :: Maybe AudiobookPublishedDay.AudiobookPublishedDay -- ^ Day the audiobook was published
  , title :: Title.Title -- ^ Book title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book story
  , audiobookAbridged :: AudiobookAbridged.AudiobookAbridged -- ^ If it's an audiobook, whether it's been abridged
  , anthology :: Anthology.Anthology -- ^ Whether it's an anthology
  , rolePlayingBook :: RolePlayingBook.RolePlayingBook -- ^ Whether it's a role playing book
  , referenceBook :: ReferenceBook.ReferenceBook -- ^ Whether it's a reference book
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the book was published
  , eBook :: EBook.EBook -- ^ Whether it's an eBook
  , uid :: Uid.Uid -- ^ Book unique ID
  , audiobook :: Audiobook.Audiobook -- ^ Whether it's an audiobook, or has been release as an audiobook in addition to other form
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the book was published
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of book story
  , novel :: Novel.Novel -- ^ Whether it's a novel
  , audiobookRunTime :: Maybe AudiobookRunTime.AudiobookRunTime -- ^ Audiobook run time, in minutes
  , novelization :: Novelization.Novelization -- ^ Whether it's a novelization
  , productionNumber :: Maybe ProductionNumber.ProductionNumber -- ^ Book's production number
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book story
  , audiobookPublishedMonth :: Maybe AudiobookPublishedMonth.AudiobookPublishedMonth -- ^ Month the audiobook was published
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of book story
  , audiobookPublishedYear :: Maybe AudiobookPublishedYear.AudiobookPublishedYear -- ^ Year the audiobook was published
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the book was published
  }
  deriving (Eq, Show)

bookBaseSchema :: FC.Fleece schema => schema BookBase
bookBaseSchema =
  FC.object $
    FC.constructor BookBase
      #+ FC.required "biographyBook" biographyBook BiographyBook.biographyBookSchema
      #+ FC.optional "audiobookPublishedDay" audiobookPublishedDay AudiobookPublishedDay.audiobookPublishedDaySchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.required "audiobookAbridged" audiobookAbridged AudiobookAbridged.audiobookAbridgedSchema
      #+ FC.required "anthology" anthology Anthology.anthologySchema
      #+ FC.required "rolePlayingBook" rolePlayingBook RolePlayingBook.rolePlayingBookSchema
      #+ FC.required "referenceBook" referenceBook ReferenceBook.referenceBookSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.required "eBook" eBook EBook.eBookSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "audiobook" audiobook Audiobook.audiobookSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.required "novel" novel Novel.novelSchema
      #+ FC.optional "audiobookRunTime" audiobookRunTime AudiobookRunTime.audiobookRunTimeSchema
      #+ FC.required "novelization" novelization Novelization.novelizationSchema
      #+ FC.optional "productionNumber" productionNumber ProductionNumber.productionNumberSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "audiobookPublishedMonth" audiobookPublishedMonth AudiobookPublishedMonth.audiobookPublishedMonthSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "audiobookPublishedYear" audiobookPublishedYear AudiobookPublishedYear.audiobookPublishedYearSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema