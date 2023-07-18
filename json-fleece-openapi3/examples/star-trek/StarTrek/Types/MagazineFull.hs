{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineFull
  ( MagazineFull(..)
  , magazineFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.MagazineFull.CoverDay as CoverDay
import qualified StarTrek.Types.MagazineFull.CoverMonth as CoverMonth
import qualified StarTrek.Types.MagazineFull.CoverYear as CoverYear
import qualified StarTrek.Types.MagazineFull.IssueNumber as IssueNumber
import qualified StarTrek.Types.MagazineFull.NumberOfPages as NumberOfPages
import qualified StarTrek.Types.MagazineFull.PublishedDay as PublishedDay
import qualified StarTrek.Types.MagazineFull.PublishedMonth as PublishedMonth
import qualified StarTrek.Types.MagazineFull.PublishedYear as PublishedYear
import qualified StarTrek.Types.MagazineFull.Title as Title
import qualified StarTrek.Types.MagazineFull.Uid as Uid
import qualified StarTrek.Types.MagazineSeriesBase as MagazineSeriesBase
import qualified StarTrek.Types.StaffBase as StaffBase

data MagazineFull = MagazineFull
  { coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , uid :: Uid.Uid -- ^ Magazine unique ID
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the magazine was published
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the magazine was published
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , magazineSeries :: Maybe [MagazineSeriesBase.MagazineSeriesBase] -- ^ Base magazine series, returned in search results
  , title :: Title.Title -- ^ Magazine title
  , issueNumber :: Maybe IssueNumber.IssueNumber -- ^ Magazine issue number
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the magazine was published
  }
  deriving (Eq, Show)

magazineFullSchema :: FC.Fleece schema => schema MagazineFull
magazineFullSchema =
  FC.object $
    FC.constructor MagazineFull
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "magazineSeries" magazineSeries (FC.list MagazineSeriesBase.magazineSeriesBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "issueNumber" issueNumber IssueNumber.issueNumberSchema
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema