{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesFull
  ( MagazineSeriesFull(..)
  , magazineSeriesFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.MagazineBase (MagazineBase, magazineBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data MagazineSeriesFull = MagazineSeriesFull
  { publishers :: Maybe [CompanyBase] -- ^ Companies that published this magazine series
  , magazines :: Maybe [MagazineBase] -- ^ Magazines included in this magazine series
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the magazine series was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the magazine series was published
  , uid :: Text -- ^ Magazine series unique ID
  , numberOfIssues :: Maybe Integer -- ^ Number of issues
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the magazine series was published
  , title :: Text -- ^ Magazine series title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the magazine series was published
  , editors :: Maybe [StaffBase] -- ^ Editors involved in the magazine series
  }
  deriving (Eq, Show)

magazineSeriesFullSchema :: FC.Fleece schema => schema MagazineSeriesFull
magazineSeriesFullSchema =
  FC.object $
    FC.constructor MagazineSeriesFull
      #+ FC.optional "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optional "magazines" magazines (FC.list magazineBaseSchema)
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "numberOfIssues" numberOfIssues FC.integer
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optional "editors" editors (FC.list staffBaseSchema)