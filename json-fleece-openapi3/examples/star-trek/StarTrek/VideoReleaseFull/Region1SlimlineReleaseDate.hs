{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.Region1SlimlineReleaseDate
  ( Region1SlimlineReleaseDate(..)
  , region1SlimlineReleaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region1SlimlineReleaseDate = Region1SlimlineReleaseDate Day
  deriving (Show, Eq)

region1SlimlineReleaseDateSchema :: FC.Fleece schema => schema Region1SlimlineReleaseDate
region1SlimlineReleaseDateSchema =
  FC.coerceSchema FC.day