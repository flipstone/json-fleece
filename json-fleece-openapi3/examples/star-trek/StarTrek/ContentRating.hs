{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentRating
  ( ContentRating(..)
  , contentRatingSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ContentRating.Rating as Rating
import qualified StarTrek.ContentRating.Uid as Uid
import qualified StarTrek.ContentRatingSystem as ContentRatingSystem

data ContentRating = ContentRating
  { uid :: Maybe Uid.Uid -- ^ Rating unique ID
  , rating :: Maybe Rating.Rating -- ^ Rating within specified content rating system
  , contentRatingSystem :: Maybe ContentRatingSystem.ContentRatingSystem -- ^ Content rating system
  }
  deriving (Eq, Show)

contentRatingSchema :: FC.Fleece schema => schema ContentRating
contentRatingSchema =
  FC.object $
    FC.constructor ContentRating
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "rating" rating Rating.ratingSchema
      #+ FC.optional "contentRatingSystem" contentRatingSystem ContentRatingSystem.contentRatingSystemSchema