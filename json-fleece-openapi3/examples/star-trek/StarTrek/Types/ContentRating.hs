{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ContentRating
  ( ContentRating(..)
  , contentRatingSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ContentRating.Rating as Rating
import qualified StarTrek.Types.ContentRating.Uid as Uid
import qualified StarTrek.Types.ContentRatingSystem as ContentRatingSystem

data ContentRating = ContentRating
  { contentRatingSystem :: Maybe ContentRatingSystem.ContentRatingSystem -- ^ Content rating system
  , uid :: Maybe Uid.Uid -- ^ Rating unique ID
  , rating :: Maybe Rating.Rating -- ^ Rating within specified content rating system
  }
  deriving (Eq, Show)

contentRatingSchema :: FC.Fleece schema => schema ContentRating
contentRatingSchema =
  FC.object $
    FC.constructor ContentRating
      #+ FC.optional "contentRatingSystem" contentRatingSystem ContentRatingSystem.contentRatingSystemSchema
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "rating" rating Rating.ratingSchema