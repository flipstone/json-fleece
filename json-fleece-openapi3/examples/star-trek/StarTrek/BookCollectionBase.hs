{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionBase
  ( BookCollectionBase(..)
  , bookCollectionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookCollectionBase.NumberOfPages as NumberOfPages
import qualified StarTrek.BookCollectionBase.PublishedDay as PublishedDay
import qualified StarTrek.BookCollectionBase.PublishedMonth as PublishedMonth
import qualified StarTrek.BookCollectionBase.PublishedYear as PublishedYear
import qualified StarTrek.BookCollectionBase.StardateFrom as StardateFrom
import qualified StarTrek.BookCollectionBase.StardateTo as StardateTo
import qualified StarTrek.BookCollectionBase.Title as Title
import qualified StarTrek.BookCollectionBase.Uid as Uid
import qualified StarTrek.BookCollectionBase.YearFrom as YearFrom
import qualified StarTrek.BookCollectionBase.YearTo as YearTo

data BookCollectionBase = BookCollectionBase
  { yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book collection stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of book collection stories
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the book collection was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the book collection was published
  , uid :: Maybe Uid.Uid -- ^ Book collection unique ID
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of book collection stories
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the book collection was published
  , title :: Maybe Title.Title -- ^ Book collection title
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book collection stories
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  }
  deriving (Eq, Show)

bookCollectionBaseSchema :: FC.Fleece schema => schema BookCollectionBase
bookCollectionBaseSchema =
  FC.object $
    FC.constructor BookCollectionBase
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "title" title Title.titleSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema