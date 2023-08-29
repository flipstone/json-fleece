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
  { magazines :: Maybe [MagazineBase.MagazineBase] -- ^ Base magazine, returned in search results
  , title :: Title.Title -- ^ Magazine series title
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the magazine series was published
  , editors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the magazine series was published
  , uid :: Uid.Uid -- ^ Magazine series unique ID
  , publishers :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the magazine series was published
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the magazine series was published
  }
  deriving (Eq, Show)

magazineSeriesFullSchema :: FC.Fleece schema => schema MagazineSeriesFull
magazineSeriesFullSchema =
  FC.object $
    FC.constructor MagazineSeriesFull
      #+ FC.optional "magazines" magazines (FC.list MagazineBase.magazineBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "editors" editors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishers" publishers (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema