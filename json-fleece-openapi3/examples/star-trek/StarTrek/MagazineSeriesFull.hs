{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesFull
  ( MagazineSeriesFull(..)
  , magazineSeriesFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.MagazineBase (MagazineBase, magazineBaseSchema)
import StarTrek.MagazineSeriesFull.NumberOfIssues (NumberOfIssues, numberOfIssuesSchema)
import StarTrek.MagazineSeriesFull.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.MagazineSeriesFull.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.MagazineSeriesFull.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.MagazineSeriesFull.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.MagazineSeriesFull.Title (Title, titleSchema)
import StarTrek.MagazineSeriesFull.Uid (Uid, uidSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data MagazineSeriesFull = MagazineSeriesFull
  { publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , magazines :: Maybe [MagazineBase] -- ^ Base magazine, returned in search results
  , publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the magazine series was published
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the magazine series was published
  , uid :: Uid -- ^ Magazine series unique ID
  , numberOfIssues :: Maybe NumberOfIssues -- ^ Number of issues
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the magazine series was published
  , title :: Title -- ^ Magazine series title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the magazine series was published
  , editors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

magazineSeriesFullSchema :: FC.Fleece schema => schema MagazineSeriesFull
magazineSeriesFullSchema =
  FC.object $
    FC.constructor MagazineSeriesFull
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "magazines" magazines (FC.list magazineBaseSchema)
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "numberOfIssues" numberOfIssues numberOfIssuesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)