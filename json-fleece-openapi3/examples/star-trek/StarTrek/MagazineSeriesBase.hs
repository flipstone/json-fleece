{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesBase
  ( MagazineSeriesBase(..)
  , magazineSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.MagazineSeriesBase.NumberOfIssues as NumberOfIssues
import qualified StarTrek.MagazineSeriesBase.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.MagazineSeriesBase.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.MagazineSeriesBase.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.MagazineSeriesBase.PublishedYearTo as PublishedYearTo
import qualified StarTrek.MagazineSeriesBase.Title as Title
import qualified StarTrek.MagazineSeriesBase.Uid as Uid

data MagazineSeriesBase = MagazineSeriesBase
  { publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the magazine series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the magazine series was published
  , uid :: Uid.Uid -- ^ Magazine series unique ID
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the magazine series was published
  , title :: Title.Title -- ^ Magazine series title
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the magazine series was published
  }
  deriving (Eq, Show)

magazineSeriesBaseSchema :: FC.Fleece schema => schema MagazineSeriesBase
magazineSeriesBaseSchema =
  FC.object $
    FC.constructor MagazineSeriesBase
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema