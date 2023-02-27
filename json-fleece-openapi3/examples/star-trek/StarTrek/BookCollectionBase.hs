{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionBase
  ( BookCollectionBase(..)
  , bookCollectionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookCollectionBase.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.BookCollectionBase.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.BookCollectionBase.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.BookCollectionBase.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.BookCollectionBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.BookCollectionBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.BookCollectionBase.Title (Title, titleSchema)
import StarTrek.BookCollectionBase.Uid (Uid, uidSchema)
import StarTrek.BookCollectionBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.BookCollectionBase.YearTo (YearTo, yearToSchema)

data BookCollectionBase = BookCollectionBase
  { yearFrom :: Maybe YearFrom -- ^ Starting year of book collection stories
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of book collection stories
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the book collection was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the book collection was published
  , uid :: Maybe Uid -- ^ Book collection unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of book collection stories
  , publishedDay :: Maybe PublishedDay -- ^ Day the book collection was published
  , title :: Maybe Title -- ^ Book collection title
  , yearTo :: Maybe YearTo -- ^ Ending year of book collection stories
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  }
  deriving (Eq, Show)

bookCollectionBaseSchema :: FC.Fleece schema => schema BookCollectionBase
bookCollectionBaseSchema =
  FC.object $
    FC.constructor BookCollectionBase
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "publishedMonth" publishedMonth publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear publishedYearSchema
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "publishedDay" publishedDay publishedDaySchema
      #+ FC.optional "title" title titleSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema