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
  { editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , magazines :: Maybe [MagazineBase.MagazineBase] -- ^ Base magazine, returned in search results
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the magazine series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the magazine series was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the magazine series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the magazine series was published
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , title :: Title.Title -- ^ Magazine series title
  , uid :: Uid.Uid -- ^ Magazine series unique ID
  }
  deriving (Eq, Show)

magazineSeriesFullSchema :: FC.Fleece schema => schema MagazineSeriesFull
magazineSeriesFullSchema =
  FC.object $
    FC.constructor MagazineSeriesFull
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "magazines" magazines (FC.list MagazineBase.magazineBaseSchema)
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema