{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.RegionFreeReleaseDate
  ( RegionFreeReleaseDate(..)
  , regionFreeReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype RegionFreeReleaseDate = RegionFreeReleaseDate Time.Day
  deriving (Show, Eq)

regionFreeReleaseDateSchema :: FC.Fleece t => FC.Schema t RegionFreeReleaseDate
regionFreeReleaseDateSchema =
  FC.coerceSchema FC.day