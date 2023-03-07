{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineSeriesFull
  ( MagazineSeriesFull(..)
  , magazineSeriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.MagazineBase as MagazineBase
import qualified StarTrek.Types.MagazineSeriesFull.NumberOfIssues as NumberOfIssues
import qualified StarTrek.Types.MagazineSeriesFull.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.MagazineSeriesFull.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.MagazineSeriesFull.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.MagazineSeriesFull.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.MagazineSeriesFull.Title as Title
import qualified StarTrek.Types.MagazineSeriesFull.Uid as Uid
import qualified StarTrek.Types.StaffBase as StaffBase

data MagazineSeriesFull = MagazineSeriesFull
  { publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , magazines :: Maybe [MagazineBase.MagazineBase] -- ^ Base magazine, returned in search results
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the magazine series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the magazine series was published
  , uid :: Uid.Uid -- ^ Magazine series unique ID
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the magazine series was published
  , title :: Title.Title -- ^ Magazine series title
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the magazine series was published
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

magazineSeriesFullSchema :: FC.Fleece schema => schema MagazineSeriesFull
magazineSeriesFullSchema =
  FC.object $
    FC.constructor MagazineSeriesFull
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "magazines" magazines (FC.list MagazineBase.magazineBaseSchema)
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)