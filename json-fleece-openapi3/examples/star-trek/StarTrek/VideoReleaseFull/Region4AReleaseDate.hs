{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.Region4AReleaseDate
  ( Region4AReleaseDate(..)
  , region4AReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region4AReleaseDate = Region4AReleaseDate Time.Day
  deriving (Show, Eq)

region4AReleaseDateSchema :: FC.Fleece schema => schema Region4AReleaseDate
region4AReleaseDateSchema =
  FC.coerceSchema FC.day