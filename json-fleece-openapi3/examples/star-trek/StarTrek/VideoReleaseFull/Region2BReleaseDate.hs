{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.Region2BReleaseDate
  ( Region2BReleaseDate(..)
  , region2BReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region2BReleaseDate = Region2BReleaseDate Time.Day
  deriving (Show, Eq)

region2BReleaseDateSchema :: FC.Fleece schema => schema Region2BReleaseDate
region2BReleaseDateSchema =
  FC.coerceSchema FC.day