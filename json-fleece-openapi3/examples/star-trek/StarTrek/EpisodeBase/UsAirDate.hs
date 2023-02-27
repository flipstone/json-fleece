{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBase.UsAirDate
  ( UsAirDate(..)
  , usAirDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype UsAirDate = UsAirDate Day
  deriving (Show, Eq)

usAirDateSchema :: FC.Fleece schema => schema UsAirDate
usAirDateSchema =
  FC.coerceSchema FC.day