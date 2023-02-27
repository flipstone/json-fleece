{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineBase
  ( MagazineBase(..)
  , magazineBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MagazineBase.CoverDay (CoverDay, coverDaySchema)
import StarTrek.MagazineBase.CoverMonth (CoverMonth, coverMonthSchema)
import StarTrek.MagazineBase.CoverYear (CoverYear, coverYearSchema)
import StarTrek.MagazineBase.IssueNumber (IssueNumber, issueNumberSchema)
import StarTrek.MagazineBase.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.MagazineBase.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.MagazineBase.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.MagazineBase.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.MagazineBase.Title (Title, titleSchema)
import StarTrek.MagazineBase.Uid (Uid, uidSchema)

data MagazineBase = MagazineBase
  { publishedMonth :: Maybe PublishedMonth -- ^ Month the magazine was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the magazine was published
  , uid :: Uid -- ^ Magazine unique ID
  , publishedDay :: Maybe PublishedDay -- ^ Day the magazine was published
  , coverYear :: Maybe CoverYear -- ^ Cover publication year
  , issueNumber :: Maybe IssueNumber -- ^ Magazine issue number
  , title :: Title -- ^ Magazine title
  , coverDay :: Maybe CoverDay -- ^ Cover publication day
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , coverMonth :: Maybe CoverMonth -- ^ Cover publication month
  }
  deriving (Eq, Show)

magazineBaseSchema :: FC.Fleece schema => schema MagazineBase
magazineBaseSchema =
  FC.object $
    FC.constructor MagazineBase
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