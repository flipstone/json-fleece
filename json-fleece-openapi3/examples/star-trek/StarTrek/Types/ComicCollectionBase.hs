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
  , title :: Title.Title -- ^ Comic collection title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic collection stories
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the comic collection was published
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , uid :: Uid.Uid -- ^ Comic collection unique ID
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the comic collection was published
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic collection stories
  , photonovel :: Maybe Photonovel.Photonovel -- ^ Whether it's a photonovel collection
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic collection stories
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic collection stories
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the comic collection was published
  }
  deriving (Eq, Show)

comicCollectionBaseSchema :: FC.Fleece schema => schema ComicCollectionBase
comicCollectionBaseSchema =
  FC.object $
    FC.constructor ComicCollectionBase
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "photonovel" photonovel Photonovel.photonovelSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema