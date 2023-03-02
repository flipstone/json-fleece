{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.Region4SlimlineReleaseDate
  ( Region4SlimlineReleaseDate(..)
  , region4SlimlineReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region4SlimlineReleaseDate = Region4SlimlineReleaseDate Time.Day
  deriving (Show, Eq)

region4SlimlineReleaseDateSchema :: FC.Fleece schema => schema Region4SlimlineReleaseDate
region4SlimlineReleaseDateSchema =
  FC.coerceSchema FC.day