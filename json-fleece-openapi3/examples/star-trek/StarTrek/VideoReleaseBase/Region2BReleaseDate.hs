{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.Region2BReleaseDate
  ( Region2BReleaseDate(..)
  , region2BReleaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region2BReleaseDate = Region2BReleaseDate Day
  deriving (Show, Eq)

region2BReleaseDateSchema :: FC.Fleece schema => schema Region2BReleaseDate
region2BReleaseDateSchema =
  FC.coerceSchema FC.day