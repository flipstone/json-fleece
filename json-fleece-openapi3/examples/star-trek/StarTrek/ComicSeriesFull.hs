{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesFull
  ( ComicSeriesFull(..)
  , comicSeriesFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ComicsBase (ComicsBase, comicsBaseSchema)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)

data ComicSeriesFull = ComicSeriesFull
  { yearFrom :: Maybe Integer -- ^ Starting year of comic series stories
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of comic series stories
  , publishers :: Maybe [CompanyBase] -- ^ Companies that published this comic series
  , publishedMonthFrom :: Maybe Integer -- ^ Month from which the comic series was published
  , publishedYearTo :: Maybe Integer -- ^ Year to which the comic series was published
  , uid :: Text -- ^ Comic series unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of comic series stories
  , numberOfIssues :: Maybe Integer -- ^ Number of issues
  , publishedYearFrom :: Maybe Integer -- ^ Year from which the comic series was published
  , parentSeries :: Maybe [ComicSeriesBase] -- ^ Comic series this comic series is included in
  , title :: Text -- ^ Comic series title
  , publishedMonthTo :: Maybe Integer -- ^ Month to which the comic series was published
  , photonovelSeries :: Maybe Bool -- ^ Whether it's a photonovel series
  , miniseries :: Maybe Bool -- ^ Whether it's a miniseries
  , yearTo :: Maybe Integer -- ^ Ending year of comic series stories
  , childSeries :: Maybe [ComicSeriesBase] -- ^ Child comic series included in this comic series
  , comics :: Maybe [ComicsBase] -- ^ Comics included in this comic series
  , publishedDayTo :: Maybe Integer -- ^ Day to which the comic series was published
  , publishedDayFrom :: Maybe Integer -- ^ Day from which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesFullSchema :: FC.Fleece schema => schema ComicSeriesFull
comicSeriesFullSchema =
  FC.object $
    FC.constructor ComicSeriesFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonthFrom" publishedMonthFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYearTo" publishedYearTo FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfIssues" numberOfIssues FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYearFrom" publishedYearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "parentSeries" parentSeries (FC.list comicSeriesBaseSchema)
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonthTo" publishedMonthTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "photonovelSeries" photonovelSeries FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "miniseries" miniseries FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "childSeries" childSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "comics" comics (FC.list comicsBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDayTo" publishedDayTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDayFrom" publishedDayFrom FC.integer