{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesBase
  ( MagazineSeriesBase(..)
  , magazineSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MagazineSeriesBase.NumberOfIssues (NumberOfIssues, numberOfIssuesSchema)
import StarTrek.MagazineSeriesBase.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.MagazineSeriesBase.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.MagazineSeriesBase.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.MagazineSeriesBase.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.MagazineSeriesBase.Title (Title, titleSchema)
import StarTrek.MagazineSeriesBase.Uid (Uid, uidSchema)

data MagazineSeriesBase = MagazineSeriesBase
  { publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the magazine series was published
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the magazine series was published
  , uid :: Uid -- ^ Magazine series unique ID
  , numberOfIssues :: Maybe NumberOfIssues -- ^ Number of issues
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the magazine series was published
  , title :: Title -- ^ Magazine series title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the magazine series was published
  }
  deriving (Eq, Show)

magazineSeriesBaseSchema :: FC.Fleece schema => schema MagazineSeriesBase
magazineSeriesBaseSchema =
  FC.object $
    FC.constructor MagazineSeriesBase
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "numberOfIssues" numberOfIssues numberOfIssuesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema