{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesBase
  ( ComicSeriesBase(..)
  , comicSeriesBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicSeriesBase.Miniseries (Miniseries, miniseriesSchema)
import StarTrek.ComicSeriesBase.NumberOfIssues (NumberOfIssues, numberOfIssuesSchema)
import StarTrek.ComicSeriesBase.PhotonovelSeries (PhotonovelSeries, photonovelSeriesSchema)
import StarTrek.ComicSeriesBase.PublishedDayFrom (PublishedDayFrom, publishedDayFromSchema)
import StarTrek.ComicSeriesBase.PublishedDayTo (PublishedDayTo, publishedDayToSchema)
import StarTrek.ComicSeriesBase.PublishedMonthFrom (PublishedMonthFrom, publishedMonthFromSchema)
import StarTrek.ComicSeriesBase.PublishedMonthTo (PublishedMonthTo, publishedMonthToSchema)
import StarTrek.ComicSeriesBase.PublishedYearFrom (PublishedYearFrom, publishedYearFromSchema)
import StarTrek.ComicSeriesBase.PublishedYearTo (PublishedYearTo, publishedYearToSchema)
import StarTrek.ComicSeriesBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.ComicSeriesBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.ComicSeriesBase.Title (Title, titleSchema)
import StarTrek.ComicSeriesBase.Uid (Uid, uidSchema)
import StarTrek.ComicSeriesBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicSeriesBase.YearTo (YearTo, yearToSchema)

data ComicSeriesBase = ComicSeriesBase
  { yearFrom :: Maybe YearFrom -- ^ Starting year of comic series stories
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of comic series stories
  , publishedMonthFrom :: Maybe PublishedMonthFrom -- ^ Month from which the comic series was published
  , publishedYearTo :: Maybe PublishedYearTo -- ^ Year to which the comic series was published
  , uid :: Uid -- ^ Comic series unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of comic series stories
  , numberOfIssues :: Maybe NumberOfIssues -- ^ Number of issues
  , publishedYearFrom :: Maybe PublishedYearFrom -- ^ Year from which the comic series was published
  , title :: Title -- ^ Comic series title
  , publishedMonthTo :: Maybe PublishedMonthTo -- ^ Month to which the comic series was published
  , photonovelSeries :: Maybe PhotonovelSeries -- ^ Whether it's a photonovel series
  , miniseries :: Maybe Miniseries -- ^ Whether it's a miniseries
  , yearTo :: Maybe YearTo -- ^ Ending year of comic series stories
  , publishedDayTo :: Maybe PublishedDayTo -- ^ Day to which the comic series was published
  , publishedDayFrom :: Maybe PublishedDayFrom -- ^ Day from which the comic series was published
  }
  deriving (Eq, Show)

comicSeriesBaseSchema :: FC.Fleece schema => schema ComicSeriesBase
comicSeriesBaseSchema =
  FC.object $
    FC.constructor ComicSeriesBase
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "publishedMonthFrom" publishedMonthFrom publishedMonthFromSchema
      #+ FC.optional "publishedYearTo" publishedYearTo publishedYearToSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "numberOfIssues" numberOfIssues numberOfIssuesSchema
      #+ FC.optional "publishedYearFrom" publishedYearFrom publishedYearFromSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "publishedMonthTo" publishedMonthTo publishedMonthToSchema
      #+ FC.optional "photonovelSeries" photonovelSeries photonovelSeriesSchema
      #+ FC.optional "miniseries" miniseries miniseriesSchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "publishedDayTo" publishedDayTo publishedDayToSchema
      #+ FC.optional "publishedDayFrom" publishedDayFrom publishedDayFromSchema