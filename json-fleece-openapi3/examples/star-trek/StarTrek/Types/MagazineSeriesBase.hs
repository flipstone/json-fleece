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
  { publishedYearTo :: Maybe PublishedYearTo.PublishedYearTo -- ^ Year to which the magazine series was published
  , publishedMonthTo :: Maybe PublishedMonthTo.PublishedMonthTo -- ^ Month to which the magazine series was published
  , numberOfIssues :: Maybe NumberOfIssues.NumberOfIssues -- ^ Number of issues
  , uid :: Uid.Uid -- ^ Magazine series unique ID
  , publishedYearFrom :: Maybe PublishedYearFrom.PublishedYearFrom -- ^ Year from which the magazine series was published
  , title :: Title.Title -- ^ Magazine series title
  , publishedMonthFrom :: Maybe PublishedMonthFrom.PublishedMonthFrom -- ^ Month from which the magazine series was published
  }
  deriving (Eq, Show)

magazineSeriesBaseSchema :: FC.Fleece schema => schema MagazineSeriesBase
magazineSeriesBaseSchema =
  FC.object $
    FC.constructor MagazineSeriesBase
      #+ FC.optional "publishedYearTo" publishedYearTo PublishedYearTo.publishedYearToSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo PublishedMonthTo.publishedMonthToSchema
      #+ FC.optional "numberOfIssues" numberOfIssues NumberOfIssues.numberOfIssuesSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom PublishedYearFrom.publishedYearFromSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom PublishedMonthFrom.publishedMonthFromSchema