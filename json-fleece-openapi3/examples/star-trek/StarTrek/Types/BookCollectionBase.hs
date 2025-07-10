{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionBase
  ( BookCollectionBase(..)
  , bookCollectionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookCollectionBase.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.BookCollectionBase.PublishedDay as PublishedDay
import qualified StarTrek.Types.BookCollectionBase.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.BookCollectionBase.PublishedYear as PublishedYear
import qualified StarTrek.Types.BookCollectionBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.BookCollectionBase.StardateTo as StardateTo
import qualified StarTrek.Types.BookCollectionBase.Title as Title
import qualified StarTrek.Types.BookCollectionBase.Uid as Uid
import qualified StarTrek.Types.BookCollectionBase.YearFrom as YearFrom
import qualified StarTrek.Types.BookCollectionBase.YearTo as YearTo

data BookCollectionBase = BookCollectionBase
  { numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the book collection was published
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the book collection was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the book collection was published
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of book collection stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of book collection stories
  , title :: Maybe Title.Title -- ^ Book collection title
  , uid :: Maybe Uid.Uid -- ^ Book collection unique ID
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of book collection stories
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of book collection stories
  }
  deriving (Eq, Show)

bookCollectionBaseSchema :: FC.Fleece schema => schema BookCollectionBase
bookCollectionBaseSchema =
  FC.object $
    FC.constructor BookCollectionBase
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "title" title Title.titleSchema
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema