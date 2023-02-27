{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBase
  ( BookBase(..)
  , bookBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookBase.Anthology (Anthology, anthologySchema)
import StarTrek.BookBase.Audiobook (Audiobook, audiobookSchema)
import StarTrek.BookBase.AudiobookAbridged (AudiobookAbridged, audiobookAbridgedSchema)
import StarTrek.BookBase.AudiobookPublishedDay (AudiobookPublishedDay, audiobookPublishedDaySchema)
import StarTrek.BookBase.AudiobookPublishedMonth (AudiobookPublishedMonth, audiobookPublishedMonthSchema)
import StarTrek.BookBase.AudiobookPublishedYear (AudiobookPublishedYear, audiobookPublishedYearSchema)
import StarTrek.BookBase.AudiobookRunTime (AudiobookRunTime, audiobookRunTimeSchema)
import StarTrek.BookBase.BiographyBook (BiographyBook, biographyBookSchema)
import StarTrek.BookBase.EBook (EBook, eBookSchema)
import StarTrek.BookBase.Novel (Novel, novelSchema)
import StarTrek.BookBase.Novelization (Novelization, novelizationSchema)
import StarTrek.BookBase.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.BookBase.ProductionNumber (ProductionNumber, productionNumberSchema)
import StarTrek.BookBase.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.BookBase.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.BookBase.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.BookBase.ReferenceBook (ReferenceBook, referenceBookSchema)
import StarTrek.BookBase.RolePlayingBook (RolePlayingBook, rolePlayingBookSchema)
import StarTrek.BookBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.BookBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.BookBase.Title (Title, titleSchema)
import StarTrek.BookBase.Uid (Uid, uidSchema)
import StarTrek.BookBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.BookBase.YearTo (YearTo, yearToSchema)

data BookBase = BookBase
  { anthology :: Anthology -- ^ Whether it's an anthology
  , yearFrom :: Maybe YearFrom -- ^ Starting year of book story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of book story
  , audiobookAbridged :: AudiobookAbridged -- ^ If it's an audiobook, whether it's been abridged
  , audiobookPublishedDay :: Maybe AudiobookPublishedDay -- ^ Day the audiobook was published
  , productionNumber :: Maybe ProductionNumber -- ^ Book's production number
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the book was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the book was published
  , uid :: Uid -- ^ Book unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of book story
  , publishedDay :: Maybe PublishedDay -- ^ Day the book was published
  , novel :: Novel -- ^ Whether it's a novel
  , audiobookRunTime :: Maybe AudiobookRunTime -- ^ Audiobook run time, in minutes
  , title :: Title -- ^ Book title
  , referenceBook :: ReferenceBook -- ^ Whether it's a reference book
  , audiobookPublishedMonth :: Maybe AudiobookPublishedMonth -- ^ Month the audiobook was published
  , yearTo :: Maybe YearTo -- ^ Ending year of book story
  , audiobookPublishedYear :: Maybe AudiobookPublishedYear -- ^ Year the audiobook was published
  , biographyBook :: BiographyBook -- ^ Whether it's a biography book
  , rolePlayingBook :: RolePlayingBook -- ^ Whether it's a role playing book
  , novelization :: Novelization -- ^ Whether it's a novelization
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , eBook :: EBook -- ^ Whether it's an eBook
  , audiobook :: Audiobook -- ^ Whether it's an audiobook, or has been release as an audiobook in addition to other form
  }
  deriving (Eq, Show)

bookBaseSchema :: FC.Fleece schema => schema BookBase
bookBaseSchema =
  FC.object $
    FC.constructor BookBase
      #+ FC.required "anthology" anthology anthologySchema
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.required "audiobookAbridged" audiobookAbridged audiobookAbridgedSchema
      #+ FC.optional "audiobookPublishedDay" audiobookPublishedDay audiobookPublishedDaySchema
      #+ FC.optional "productionNumber" productionNumber productionNumberSchema
      #+ FC.optional "publishedMonth" publishedMonth publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear publishedYearSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "publishedDay" publishedDay publishedDaySchema
      #+ FC.required "novel" novel novelSchema
      #+ FC.optional "audiobookRunTime" audiobookRunTime audiobookRunTimeSchema
      #+ FC.required "title" title titleSchema
      #+ FC.required "referenceBook" referenceBook referenceBookSchema
      #+ FC.optional "audiobookPublishedMonth" audiobookPublishedMonth audiobookPublishedMonthSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "audiobookPublishedYear" audiobookPublishedYear audiobookPublishedYearSchema
      #+ FC.required "biographyBook" biographyBook biographyBookSchema
      #+ FC.required "rolePlayingBook" rolePlayingBook rolePlayingBookSchema
      #+ FC.required "novelization" novelization novelizationSchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.required "eBook" eBook eBookSchema
      #+ FC.required "audiobook" audiobook audiobookSchema