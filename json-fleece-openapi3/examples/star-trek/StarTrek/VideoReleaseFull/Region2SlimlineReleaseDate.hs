{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.Region2SlimlineReleaseDate
  ( Region2SlimlineReleaseDate(..)
  , region2SlimlineReleaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region2SlimlineReleaseDate = Region2SlimlineReleaseDate Day
  deriving (Show, Eq)

region2SlimlineReleaseDateSchema :: FC.Fleece schema => schema Region2SlimlineReleaseDate
region2SlimlineReleaseDateSchema =
  FC.coerceSchema FC.day