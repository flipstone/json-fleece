{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineBase
  ( MagazineBase(..)
  , magazineBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MagazineBase.CoverDay as CoverDay
import qualified StarTrek.Types.MagazineBase.CoverMonth as CoverMonth
import qualified StarTrek.Types.MagazineBase.CoverYear as CoverYear
import qualified StarTrek.Types.MagazineBase.IssueNumber as IssueNumber
import qualified StarTrek.Types.MagazineBase.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.MagazineBase.PublishedDay as PublishedDay
import qualified StarTrek.Types.MagazineBase.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.MagazineBase.PublishedYear as PublishedYear
import qualified StarTrek.Types.MagazineBase.Title as Title
import qualified StarTrek.Types.MagazineBase.Uid as Uid

data MagazineBase = MagazineBase
  { publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the magazine was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the magazine was published
  , uid :: Uid.Uid -- ^ Magazine unique ID
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the magazine was published
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , issueNumber :: Maybe IssueNumber.IssueNumber -- ^ Magazine issue number
  , title :: Title.Title -- ^ Magazine title
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  }
  deriving (Eq, Show)

magazineBaseSchema :: FC.Fleece schema => schema MagazineBase
magazineBaseSchema =
  FC.object $
    FC.constructor MagazineBase
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.optional "issueNumber" issueNumber IssueNumber.issueNumberSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema