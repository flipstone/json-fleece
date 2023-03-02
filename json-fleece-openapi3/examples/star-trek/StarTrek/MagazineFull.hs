{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineFull
  ( MagazineFull(..)
  , magazineFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CompanyBase as CompanyBase
import qualified StarTrek.MagazineFull.CoverDay as CoverDay
import qualified StarTrek.MagazineFull.CoverMonth as CoverMonth
import qualified StarTrek.MagazineFull.CoverYear as CoverYear
import qualified StarTrek.MagazineFull.IssueNumber as IssueNumber
import qualified StarTrek.MagazineFull.NumberOfPages as NumberOfPages
import qualified StarTrek.MagazineFull.PublishedDay as PublishedDay
import qualified StarTrek.MagazineFull.PublishedMonth as PublishedMonth
import qualified StarTrek.MagazineFull.PublishedYear as PublishedYear
import qualified StarTrek.MagazineFull.Title as Title
import qualified StarTrek.MagazineFull.Uid as Uid
import qualified StarTrek.MagazineSeriesBase as MagazineSeriesBase
import qualified StarTrek.StaffBase as StaffBase

data MagazineFull = MagazineFull
  { magazineSeries :: Maybe [MagazineSeriesBase.MagazineSeriesBase] -- ^ Base magazine series, returned in search results
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the magazine was published
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the magazine was published
  , uid :: Uid.Uid -- ^ Magazine unique ID
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the magazine was published
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , issueNumber :: Maybe IssueNumber.IssueNumber -- ^ Magazine issue number
  , title :: Title.Title -- ^ Magazine title
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , coverMonth :: Maybe CoverMonth.CoverMonth -- ^ Cover publication month
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

magazineFullSchema :: FC.Fleece schema => schema MagazineFull
magazineFullSchema =
  FC.object $
    FC.constructor MagazineFull
      #+ FC.optional "magazineSeries" magazineSeries (FC.list MagazineSeriesBase.magazineSeriesBaseSchema)
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
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
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)