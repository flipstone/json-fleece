{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentRating
  ( ContentRating(..)
  , contentRatingSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ContentRatingSystem (ContentRatingSystem, contentRatingSystemSchema)

data ContentRating = ContentRating
  { uid :: Maybe Text -- ^ Rating unique ID
  , rating :: Maybe Text -- ^ Rating within specified content rating system
  , contentRatingSystem :: Maybe ContentRatingSystem -- ^ Content rating system
  }
  deriving (Eq, Show)

contentRatingSchema :: FC.Fleece schema => schema ContentRating
contentRatingSchema =
  FC.object $
    FC.constructor ContentRating
      #+ FC.optionalField FC.OmitKey_DelegateNull "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "rating" rating FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "contentRatingSystem" contentRatingSystem contentRatingSystemSchema