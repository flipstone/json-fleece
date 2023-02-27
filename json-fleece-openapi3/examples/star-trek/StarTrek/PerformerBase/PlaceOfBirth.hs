{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase.PlaceOfBirth
  ( PlaceOfBirth(..)
  , placeOfBirthSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PlaceOfBirth = PlaceOfBirth Text
  deriving (Show, Eq)

placeOfBirthSchema :: FC.Fleece schema => schema PlaceOfBirth
placeOfBirthSchema =
  FC.coerceSchema FC.text