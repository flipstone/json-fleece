{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionBase
  ( ComicCollectionBase(..)
  , comicCollectionBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)

data ComicCollectionBase = ComicCollectionBase
  { yearFrom :: Maybe Integer -- ^ Starting year of comic collection stories
  , stardateTo :: Maybe Scientific -- ^ Ending stardate of comic collection stories
  , publishedMonth :: Maybe Integer -- ^ Month the comic collection was published
  , publishedYear :: Maybe Integer -- ^ Year the comic collection was published
  , uid :: Text -- ^ Comic collection unique ID
  , stardateFrom :: Maybe Scientific -- ^ Starting stardate of comic collection stories
  , publishedDay :: Maybe Integer -- ^ Day the comic collection was published
  , photonovel :: Maybe Bool -- ^ Whether it's a photonovel collection
  , coverYear :: Maybe Integer -- ^ Cover publication year
  , title :: Text -- ^ Comic collection title
  , coverDay :: Maybe Integer -- ^ Cover publication day
  , yearTo :: Maybe Integer -- ^ Ending year of comic collection stories
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , coverMonth :: Maybe Integer -- ^ Cover publication month
  }
  deriving (Eq, Show)

comicCollectionBaseSchema :: FC.Fleece schema => schema ComicCollectionBase
comicCollectionBaseSchema =
  FC.object $
    FC.constructor ComicCollectionBase
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearFrom" yearFrom FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateTo" stardateTo FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonth" publishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stardateFrom" stardateFrom FC.number
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDay" publishedDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "photonovel" photonovel FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverYear" coverYear FC.integer
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverDay" coverDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "yearTo" yearTo FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverMonth" coverMonth FC.integer