{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionBase
  ( BookCollectionBase(..)
  , bookCollectionBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Float, Integer, Maybe, Show)

data BookCollectionBase = BookCollectionBase
  { yearFrom :: Maybe Integer -- ^ Starting year of book collection stories
  , stardateTo :: Maybe Float -- ^ Ending stardate of book collection stories
  , publishedMonth :: Maybe Integer -- ^ Month the book collection was published
  , publishedYear :: Maybe Integer -- ^ Year the book collection was published
  , uid :: Maybe Text -- ^ Book collection unique ID
  , stardateFrom :: Maybe Float -- ^ Starting stardate of book collection stories
  , publishedDay :: Maybe Integer -- ^ Day the book collection was published
  , title :: Maybe Text -- ^ Book collection title
  , yearTo :: Maybe Integer -- ^ Ending year of book collection stories
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  }
  deriving (Eq, Show)

bookCollectionBaseSchema :: FC.Fleece schema => schema BookCollectionBase
bookCollectionBaseSchema =
  FC.object $
    FC.constructor BookCollectionBase
      #+ FC.optional "yearFrom" yearFrom FC.integer
      #+ FC.optional "stardateTo" stardateTo FC.float
      #+ FC.optional "publishedMonth" publishedMonth FC.integer
      #+ FC.optional "publishedYear" publishedYear FC.integer
      #+ FC.optional "uid" uid FC.text
      #+ FC.optional "stardateFrom" stardateFrom FC.float
      #+ FC.optional "publishedDay" publishedDay FC.integer
      #+ FC.optional "title" title FC.text
      #+ FC.optional "yearTo" yearTo FC.integer
      #+ FC.optional "numberOfPages" numberOfPages FC.integer