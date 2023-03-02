{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentRating.Rating
  ( Rating(..)
  , ratingSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Rating = Rating T.Text
  deriving (Show, Eq)

ratingSchema :: FC.Fleece schema => schema Rating
ratingSchema =
  FC.coerceSchema FC.text