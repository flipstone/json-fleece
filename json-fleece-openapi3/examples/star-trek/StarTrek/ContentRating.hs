{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentRating
  ( ContentRating(..)
  , contentRatingSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ContentRating.Rating (Rating, ratingSchema)
import StarTrek.ContentRating.Uid (Uid, uidSchema)
import StarTrek.ContentRatingSystem (ContentRatingSystem, contentRatingSystemSchema)

data ContentRating = ContentRating
  { uid :: Maybe Uid -- ^ Rating unique ID
  , rating :: Maybe Rating -- ^ Rating within specified content rating system
  , contentRatingSystem :: Maybe ContentRatingSystem -- ^ Content rating system
  }
  deriving (Eq, Show)

contentRatingSchema :: FC.Fleece schema => schema ContentRating
contentRatingSchema =
  FC.object $
    FC.constructor ContentRating
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "rating" rating ratingSchema
      #+ FC.optional "contentRatingSystem" contentRatingSystem contentRatingSystemSchema