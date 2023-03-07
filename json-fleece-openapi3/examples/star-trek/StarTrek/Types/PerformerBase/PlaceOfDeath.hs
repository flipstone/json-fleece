{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.PlaceOfDeath
  ( PlaceOfDeath(..)
  , placeOfDeathSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PlaceOfDeath = PlaceOfDeath T.Text
  deriving (Show, Eq)

placeOfDeathSchema :: FC.Fleece schema => schema PlaceOfDeath
placeOfDeathSchema =
  FC.coerceSchema FC.text