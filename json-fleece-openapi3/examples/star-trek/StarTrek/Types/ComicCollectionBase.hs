{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicCollectionBase
  ( ComicCollectionBase(..)
  , comicCollectionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicCollectionBase.CoverDay as CoverDay
import qualified StarTrek.Types.ComicCollectionBase.CoverMonth as CoverMonth
import qualified StarTrek.Types.ComicCollectionBase.CoverYear as CoverYear
import qualified StarTrek.Types.ComicCollectionBase.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.ComicCollectionBase.Photonovel as Photonovel
import qualified StarTrek.Types.ComicCollectionBase.PublishedDay as PublishedDay
import qualified StarTrek.Types.ComicCollectionBase.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.ComicCollectionBase.PublishedYear as PublishedYear
import qualified StarTrek.Types.ComicCollectionBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.ComicCollectionBase.StardateTo as StardateTo
import qualified StarTrek.Types.ComicCollectionBase.Title as Title
import qualified StarTrek.Types.ComicCollectionBase.Uid as Uid
import qualified StarTrek.Types.ComicCollectionBase.YearFrom as YearFrom
import qualified StarTrek.Types.ComicCollectionBase.YearTo as YearTo

data ComicCollectionBase = ComicCollectionBase
  { coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic collection stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic collection stories
  , uid :: Uid.Uid -- ^ Comic collection unique ID
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the comic collection was published
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the comic collection was published
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic collection stories
  , title :: Title.Title -- ^ Comic collection title
  , photonovel :: Maybe Photonovel.Photonovel -- ^ Whether it's a photonovel collection
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic collection stories
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the comic collection was published
  }
  deriving (Eq, Show)

comicCollectionBaseSchema :: FC.Fleece schema => schema ComicCollectionBase
comicCollectionBaseSchema =
  FC.object $
    FC.constructor ComicCollectionBase
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "photonovel" photonovel Photonovel.photonovelSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema