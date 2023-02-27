{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsBase
  ( ComicsBase(..)
  , comicsBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicsBase.Adaptation (Adaptation, adaptationSchema)
import StarTrek.ComicsBase.CoverDay (CoverDay, coverDaySchema)
import StarTrek.ComicsBase.CoverMonth (CoverMonth, coverMonthSchema)
import StarTrek.ComicsBase.CoverYear (CoverYear, coverYearSchema)
import StarTrek.ComicsBase.NumberOfPages (NumberOfPages, numberOfPagesSchema)
import StarTrek.ComicsBase.Photonovel (Photonovel, photonovelSchema)
import StarTrek.ComicsBase.PublishedDay (PublishedDay, publishedDaySchema)
import StarTrek.ComicsBase.PublishedMonth (PublishedMonth, publishedMonthSchema)
import StarTrek.ComicsBase.PublishedYear (PublishedYear, publishedYearSchema)
import StarTrek.ComicsBase.StardateFrom (StardateFrom, stardateFromSchema)
import StarTrek.ComicsBase.StardateTo (StardateTo, stardateToSchema)
import StarTrek.ComicsBase.Title (Title, titleSchema)
import StarTrek.ComicsBase.Uid (Uid, uidSchema)
import StarTrek.ComicsBase.YearFrom (YearFrom, yearFromSchema)
import StarTrek.ComicsBase.YearTo (YearTo, yearToSchema)

data ComicsBase = ComicsBase
  { yearFrom :: Maybe YearFrom -- ^ Starting year of comic story
  , stardateTo :: Maybe StardateTo -- ^ Ending stardate of comic story
  , adaptation :: Maybe Adaptation -- ^ Whether it's an adaptation of an episode or a movie
  , publishedMonth :: Maybe PublishedMonth -- ^ Month the comics was published
  , publishedYear :: Maybe PublishedYear -- ^ Year the comics was published
  , uid :: Uid -- ^ Comics unique ID
  , stardateFrom :: Maybe StardateFrom -- ^ Starting stardate of comic story
  , publishedDay :: Maybe PublishedDay -- ^ Day the comics was published
  , photonovel :: Maybe Photonovel -- ^ Whether it's a photonovel
  , coverYear :: Maybe CoverYear -- ^ Cover publication year
  , title :: Title -- ^ Comics title
  , coverDay :: Maybe CoverDay -- ^ Cover publication day
  , yearTo :: Maybe YearTo -- ^ Ending year of comic story
  , numberOfPages :: Maybe NumberOfPages -- ^ Number of pages
  , coverMonth :: Maybe CoverMonth -- ^ Cover publication month
  }
  deriving (Eq, Show)

comicsBaseSchema :: FC.Fleece schema => schema ComicsBase
comicsBaseSchema =
  FC.object $
    FC.constructor ComicsBase
      #+ FC.optional "yearFrom" yearFrom yearFromSchema
      #+ FC.optional "stardateTo" stardateTo stardateToSchema
      #+ FC.optional "adaptation" adaptation adaptationSchema
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