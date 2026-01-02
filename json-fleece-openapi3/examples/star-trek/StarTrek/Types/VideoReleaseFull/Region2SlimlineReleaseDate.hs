{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.Region2SlimlineReleaseDate
  ( Region2SlimlineReleaseDate(..)
  , region2SlimlineReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region2SlimlineReleaseDate = Region2SlimlineReleaseDate Time.Day
  deriving (Show, Eq)

region2SlimlineReleaseDateSchema :: FC.Fleece t => FC.Schema t Region2SlimlineReleaseDate
region2SlimlineReleaseDateSchema =
  FC.coerceSchema FC.day