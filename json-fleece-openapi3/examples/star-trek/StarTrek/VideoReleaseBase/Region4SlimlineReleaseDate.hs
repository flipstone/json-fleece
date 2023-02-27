{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.Region4SlimlineReleaseDate
  ( Region4SlimlineReleaseDate(..)
  , region4SlimlineReleaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region4SlimlineReleaseDate = Region4SlimlineReleaseDate Day
  deriving (Show, Eq)

region4SlimlineReleaseDateSchema :: FC.Fleece schema => schema Region4SlimlineReleaseDate
region4SlimlineReleaseDateSchema =
  FC.coerceSchema FC.day