{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsBase
  ( ComicsBase(..)
  , comicsBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicsBase.Adaptation as Adaptation
import qualified StarTrek.Types.ComicsBase.CoverDay as CoverDay
import qualified StarTrek.Types.ComicsBase.CoverMonth as CoverMonth
import qualified StarTrek.Types.ComicsBase.CoverYear as CoverYear
import qualified StarTrek.Types.ComicsBase.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.ComicsBase.Photonovel as Photonovel
import qualified StarTrek.Types.ComicsBase.PublishedDay as PublishedDay
import qualified StarTrek.Types.ComicsBase.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.ComicsBase.PublishedYear as PublishedYear
import qualified StarTrek.Types.ComicsBase.StardateFrom as StardateFrom
import qualified StarTrek.Types.ComicsBase.StardateTo as StardateTo
import qualified StarTrek.Types.ComicsBase.Title as Title
import qualified StarTrek.Types.ComicsBase.Uid as Uid
import qualified StarTrek.Types.ComicsBase.YearFrom as YearFrom
import qualified StarTrek.Types.ComicsBase.YearTo as YearTo

data ComicsBase = ComicsBase
  { coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  , title :: Title.Title -- ^ Comics title
  , yearFrom :: Maybe YearFrom.YearFrom -- ^ Starting year of comic story
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the comics was published
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , uid :: Uid.Uid -- ^ Comics unique ID
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , adaptation :: Maybe Adaptation.Adaptation -- ^ Whether it's an adaptation of an episode or a movie
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the comics was published
  , stardateFrom :: Maybe StardateFrom.StardateFrom -- ^ Starting stardate of comic story
  , photonovel :: Maybe Photonovel.Photonovel -- ^ Whether it's a photonovel
  , yearTo :: Maybe YearTo.YearTo -- ^ Ending year of comic story
  , stardateTo :: Maybe StardateTo.StardateTo -- ^ Ending stardate of comic story
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the comics was published
  }
  deriving (Eq, Show)

comicsBaseSchema :: FC.Fleece schema => schema ComicsBase
comicsBaseSchema =
  FC.object $
    FC.constructor ComicsBase
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "yearFrom" yearFrom YearFrom.yearFromSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "adaptation" adaptation Adaptation.adaptationSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "stardateFrom" stardateFrom StardateFrom.stardateFromSchema
      #+ FC.optional "photonovel" photonovel Photonovel.photonovelSchema
      #+ FC.optional "yearTo" yearTo YearTo.yearToSchema
      #+ FC.optional "stardateTo" stardateTo StardateTo.stardateToSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema