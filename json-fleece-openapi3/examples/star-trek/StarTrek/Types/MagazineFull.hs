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
  , title :: Title.Title -- ^ Magazine title
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , publishedDay :: Maybe PublishedDay.PublishedDay -- ^ Day the magazine was published
  , coverYear :: Maybe CoverYear.CoverYear -- ^ Cover publication year
  , uid :: Uid.Uid -- ^ Magazine unique ID
  , magazineSeries :: Maybe [MagazineSeriesBase.MagazineSeriesBase] -- ^ Base magazine series, returned in search results
  , numberOfPages :: Maybe NumberOfPages.NumberOfPages -- ^ Number of pages
  , coverDay :: Maybe CoverDay.CoverDay -- ^ Cover publication day
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , publishedYear :: Maybe PublishedYear.PublishedYear -- ^ Year the magazine was published
  , issueNumber :: Maybe IssueNumber.IssueNumber -- ^ Magazine issue number
  , publishedMonth :: Maybe PublishedMonth.PublishedMonth -- ^ Month the magazine was published
  }
  deriving (Eq, Show)

magazineFullSchema :: FC.Fleece schema => schema MagazineFull
magazineFullSchema =
  FC.object $
    FC.constructor MagazineFull
      #+ FC.optional "coverMonth" coverMonth CoverMonth.coverMonthSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "publishedDay" publishedDay PublishedDay.publishedDaySchema
      #+ FC.optional "coverYear" coverYear CoverYear.coverYearSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "magazineSeries" magazineSeries (FC.list MagazineSeriesBase.magazineSeriesBaseSchema)
      #+ FC.optional "numberOfPages" numberOfPages NumberOfPages.numberOfPagesSchema
      #+ FC.optional "coverDay" coverDay CoverDay.coverDaySchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "publishedYear" publishedYear PublishedYear.publishedYearSchema
      #+ FC.optional "issueNumber" issueNumber IssueNumber.issueNumberSchema
      #+ FC.optional "publishedMonth" publishedMonth PublishedMonth.publishedMonthSchema