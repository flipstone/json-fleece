{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineFull
  ( MagazineFull(..)
  , magazineFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.MagazineFull.CoverDay (CoverDay, coverDaySchema)
import StarTrek.MagazineFull.CoverMonth (CoverMonth, coverMonthSchema)
import StarTrek.MagazineFull.CoverYear (CoverYear, coverYearSchema)
import StarTrek.MagazineFull.IssueNumber (IssueNumber, issueNumberSchema)
import StarTrek.MagazineFull.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.MagazineFull.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.MagazineFull.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.MagazineFull.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.MagazineFull.Title (Title, titleSchema)
import StarTrek.MagazineFull.Uid (Uid, uidSchema)
import StarTrek.MagazineSeriesBase (MagazineSeriesBase, magazineSeriesBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data MagazineFull = MagazineFull
  { magazineSeries :: Maybe [MagazineSeriesBase] -- ^ Base magazine series, returned in search results
  , publishers :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the magazine was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the magazine was published
  , uid :: Uid -- ^ Magazine unique ID
  , publishedDay :: Maybe PublishedDay -- ^ Day the magazine was published
  , coverYear :: Maybe CoverYear -- ^ Cover publication year
  , issueNumber :: Maybe IssueNumber -- ^ Magazine issue number
  , title :: Title -- ^ Magazine title
  , coverDay :: Maybe CoverDay -- ^ Cover publication day
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , coverMonth :: Maybe CoverMonth -- ^ Cover publication month
  , editors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

magazineFullSchema :: FC.Fleece schema => schema MagazineFull
magazineFullSchema =
  FC.object $
    FC.constructor MagazineFull
      #+ FC.optional "magazineSeries" magazineSeries (FC.list magazineSeriesBaseSchema)
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "publishedMonth" publishedMonth publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear publishedYearSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "publishedDay" publishedDay publishedDaySchema
      #+ FC.optional "coverYear" coverYear coverYearSchema
      #+ FC.optional "issueNumber" issueNumber issueNumberSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "coverDay" coverDay coverDaySchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.optional "coverMonth" coverMonth coverMonthSchema
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)