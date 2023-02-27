{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsBase
  ( ComicsBase(..)
  , comicsBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data ComicsBase = ComicsBase
  { yearFrom :: Maybe Integer -- ^ Starting year of comic story
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of comic story
  , adaptation :: Maybe Bool -- ^ Whether it's an adaptation of an episode or a movie
  , publishedMonth :: Maybe Integer -- ^ Month the comics was published
  , publishedYear :: Maybe Integer -- ^ Year the comics was published
  , uid :: Text -- ^ Comics unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of comic story
  , publishedDay :: Maybe Integer -- ^ Day the comics was published
  , photonovel :: Maybe Bool -- ^ Whether it's a photonovel
  , coverYear :: Maybe Integer -- ^ Cover publication year
  , title :: Text -- ^ Comics title
  , coverDay :: Maybe Integer -- ^ Cover publication day
  , yearTo :: Maybe Integer -- ^ Ending year of comic story
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , coverMonth :: Maybe Integer -- ^ Cover publication month
  }
  deriving (Eq, Show)

comicsBaseSchema :: FC.Fleece schema => schema ComicsBase
comicsBaseSchema =
  FC.object $
    FC.constructor ComicsBase
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.number
      #+ FC.optional "adaptation" adaptation FC.boolean
      #+ FC.optional "publishedMonth" publishedMonth FC.integer
      #+ FC.optional "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.number
      #+ FC.optional "publishedDay" publishedDay FC.integer
      #+ FC.optional "photonovel" photonovel FC.boolean
      #+ FC.optional "coverYear" coverYear FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optional "coverDay" coverDay FC.integer
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.optional "coverMonth" coverMonth FC.integer