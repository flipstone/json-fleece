{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.Region1AReleaseDate
  ( Region1AReleaseDate(..)
  , region1AReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region1AReleaseDate = Region1AReleaseDate Time.Day
  deriving (Show, Eq)

region1AReleaseDateSchema :: FC.Fleece schema => schema Region1AReleaseDate
region1AReleaseDateSchema =
  FC.coerceSchema FC.day