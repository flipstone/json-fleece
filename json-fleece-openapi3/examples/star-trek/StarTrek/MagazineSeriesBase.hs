{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesBase
  ( MagazineSeriesBase(..)
  , magazineSeriesBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)

data MagazineSeriesBase = MagazineSeriesBase
  { publishedMonthFrom :: Maybe Integer -- ^ Month from which the magazine series was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the magazine series was published
  , uid :: Text -- ^ Magazine series unique ID
  , numberOfIssues :: Maybe Integer -- ^ Number of issues
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the magazine series was published
  , title :: Text -- ^ Magazine series title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the magazine series was published
  }
  deriving (Eq, Show)

magazineSeriesBaseSchema :: FC.Fleece schema => schema MagazineSeriesBase
magazineSeriesBaseSchema =
  FC.object $
    FC.constructor MagazineSeriesBase
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optional "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "numberOfIssues" numberOfIssues FC.integer
      #+ FC.optional "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "publishedMonthTo" publishedMonthTo FC.integer