{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.Region4AReleaseDate
  ( Region4AReleaseDate(..)
  , region4AReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region4AReleaseDate = Region4AReleaseDate Time.Day
  deriving (Show, Eq)

region4AReleaseDateSchema :: FC.Fleece t => FC.Schema t Region4AReleaseDate
region4AReleaseDateSchema =
  FC.coerceSchema FC.day