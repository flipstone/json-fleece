{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.Region1AReleaseDate
  ( Region1AReleaseDate(..)
  , region1AReleaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Region1AReleaseDate = Region1AReleaseDate Day
  deriving (Show, Eq)

region1AReleaseDateSchema :: FC.Fleece schema => schema Region1AReleaseDate
region1AReleaseDateSchema =
  FC.coerceSchema FC.day