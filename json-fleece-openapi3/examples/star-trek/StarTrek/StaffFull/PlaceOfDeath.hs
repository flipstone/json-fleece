{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.PlaceOfDeath
  ( PlaceOfDeath(..)
  , placeOfDeathSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PlaceOfDeath = PlaceOfDeath Text
  deriving (Show, Eq)

placeOfDeathSchema :: FC.Fleece schema => schema PlaceOfDeath
placeOfDeathSchema =
  FC.coerceSchema FC.text