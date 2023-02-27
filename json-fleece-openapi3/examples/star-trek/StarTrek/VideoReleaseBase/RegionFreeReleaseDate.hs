{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.RegionFreeReleaseDate
  ( RegionFreeReleaseDate(..)
  , regionFreeReleaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype RegionFreeReleaseDate = RegionFreeReleaseDate Day
  deriving (Show, Eq)

regionFreeReleaseDateSchema :: FC.Fleece schema => schema RegionFreeReleaseDate
regionFreeReleaseDateSchema =
  FC.coerceSchema FC.day