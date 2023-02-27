{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionBase
  ( ComicCollectionBase(..)
  , comicCollectionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicCollectionBase.CoverDay (CoverDay, coverDaySchema)
import StarTrek.ComicCollectionBase.CoverMonth (CoverMonth, coverMonthSchema)
import StarTrek.ComicCollectionBase.CoverYear (CoverYear, coverYearSchema)
import StarTrek.ComicCollectionBase.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.ComicCollectionBase.Photonovel (Photonovel, photonovelSchema)
import StarTrek.ComicCollectionBase.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.ComicCollectionBase.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.ComicCollectionBase.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.ComicCollectionBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.ComicCollectionBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.ComicCollectionBase.Title (Title, titleSchema)
import StarTrek.ComicCollectionBase.Uid (Uid, uidSchema)
import StarTrek.ComicCollectionBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicCollectionBase.YearTo (YearTo, yearToSchema)

data ComicCollectionBase = ComicCollectionBase
  { yearFrom :: Maybe YearFrom -- ^ Starting year of comic collection stories
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of comic collection stories
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the comic collection was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the comic collection was published
  , uid :: Uid -- ^ Comic collection unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of comic collection stories
  , publishedDay :: Maybe PublishedDay -- ^ Day the comic collection was published
  , photonovel :: Maybe Photonovel -- ^ Whether it's a photonovel collection
  , coverYear :: Maybe CoverYear -- ^ Cover publication year
  , title :: Title -- ^ Comic collection title
  , coverDay :: Maybe CoverDay -- ^ Cover publication day
  , yearTo :: Maybe YearTo -- ^ Ending year of comic collection stories
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , coverMonth :: Maybe CoverMonth -- ^ Cover publication month
  }
  deriving (Eq, Show)

comicCollectionBaseSchema :: FC.Fleece schema => schema ComicCollectionBase
comicCollectionBaseSchema =
  FC.object $
    FC.constructor ComicCollectionBase
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "publishedMonth" publishedMonth publishedMonthSchema
      #+ FC.optional "publishedYear" publishedYear publishedYearSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "stardateFrom" stardateFrom stardateFromSchema
      #+ FC.optional "publishedDay" publishedDay publishedDaySchema
      #+ FC.optional "photonovel" photonovel photonovelSchema
      #+ FC.optional "coverYear" coverYear coverYearSchema
      #+ FC.required "title" title titleSchema
      #+ FC.optional "coverDay" coverDay coverDaySchema
      #+ FC.optional "yearTo" yearTo yearToSchema
      #+ FC.optional "numberOfPages" numberOfPages numberOfPagesSchema
      #+ FC.optional "coverMonth" coverMonth coverMonthSchema