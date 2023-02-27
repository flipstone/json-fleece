{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull.UsReleaseDate
  ( UsReleaseDate(..)
  , usReleaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype UsReleaseDate = UsReleaseDate Day
  deriving (Show, Eq)

usReleaseDateSchema :: FC.Fleece schema => schema UsReleaseDate
usReleaseDateSchema =
  FC.coerceSchema FC.day