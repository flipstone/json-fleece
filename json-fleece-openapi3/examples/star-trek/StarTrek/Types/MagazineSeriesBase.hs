{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineSeriesBase
  ( MagazineSeriesBase(..)
  , magazineSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MagazineSeriesBase.NumberOfIssues as NumberOfIssues
import qualified StarTrek.Types.MagazineSeriesBase.PublishedMonthFrom as PublishedMonthFrom
import qualified StarTrek.Types.MagazineSeriesBase.PublishedMonthTo as PublishedMonthTo
import qualified StarTrek.Types.MagazineSeriesBase.PublishedYearFrom as PublishedYearFrom
import qualified StarTrek.Types.MagazineSeriesBase.PublishedYearTo as PublishedYearTo
import qualified StarTrek.Types.MagazineSeriesBase.Title as Title
import qualified StarTrek.Types.MagazineSeriesBase.Uid as Uid

data MagazineSeriesBase = MagazineSeriesBase
  { numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the magazine series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the magazine series was published
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the magazine series was published
  , publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the magazine series was published
  , title :: Title.Title -- ^ Magazine series title
  , uid :: Uid.Uid -- ^ Magazine series unique ID
  }
  deriving (Eq, Show)

magazineSeriesBaseSchema :: FC.Fleece t => FC.Schema t MagazineSeriesBase
magazineSeriesBaseSchema =
  FC.object $
    FC.constructor MagazineSeriesBase
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema