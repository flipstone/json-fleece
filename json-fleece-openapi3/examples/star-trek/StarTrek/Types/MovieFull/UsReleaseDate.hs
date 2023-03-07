{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull.UsReleaseDate
  ( UsReleaseDate(..)
  , usReleaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype UsReleaseDate = UsReleaseDate Time.Day
  deriving (Show, Eq)

usReleaseDateSchema :: FC.Fleece schema => schema UsReleaseDate
usReleaseDateSchema =
  FC.coerceSchema FC.day