{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.Region1SlimlineReleaseDate
  ( Region1SlimlineReleaseDate(..)
  , region1SlimlineReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region1SlimlineReleaseDate = Region1SlimlineReleaseDate Time.Day
  deriving (Show, Eq)

region1SlimlineReleaseDateSchema :: FC.Fleece schema => schema Region1SlimlineReleaseDate
region1SlimlineReleaseDateSchema =
  FC.coerceSchema FC.day