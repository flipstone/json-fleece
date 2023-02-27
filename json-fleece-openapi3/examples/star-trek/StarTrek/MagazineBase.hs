{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineBase
  ( MagazineBase(..)
  , magazineBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Maybe, Show)

data MagazineBase = MagazineBase
  { publishedMonth :: Maybe Integer -- ^ Month the magazine was published
  , publishedYear :: Maybe Integer -- ^ Year the magazine was published
  , uid :: Text -- ^ Magazine unique ID
  , publishedDay :: Maybe Integer -- ^ Day the magazine was published
  , coverYear :: Maybe Integer -- ^ Cover publication year
  , issueNumber :: Maybe Text -- ^ Magazine issue number
  , title :: Text -- ^ Magazine title
  , coverDay :: Maybe Integer -- ^ Cover publication day
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , coverMonth :: Maybe Integer -- ^ Cover publication month
  }
  deriving (Eq, Show)

magazineBaseSchema :: FC.Fleece schema => schema MagazineBase
magazineBaseSchema =
  FC.object $
    FC.constructor MagazineBase
      #+ FC.optional "publishedMonth" publishedMonth FC.integer
      #+ FC.optional "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "publishedDay" publishedDay FC.integer
      #+ FC.optional "coverYear" coverYear FC.integer
      #+ FC.optional "issueNumber" issueNumber FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optional "coverDay" coverDay FC.integer
      #+ FC.optional "numberOfPages" numberOfPages FC.integer
      #+ FC.optional "coverMonth" coverMonth FC.integer