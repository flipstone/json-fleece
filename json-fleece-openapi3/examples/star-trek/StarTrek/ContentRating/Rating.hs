{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentRating.Rating
  ( Rating(..)
  , ratingSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Rating = Rating Text
  deriving (Show, Eq)

ratingSchema :: FC.Fleece schema => schema Rating
ratingSchema =
  FC.coerceSchema FC.text