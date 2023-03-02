{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalFull.EarthAnimal
  ( EarthAnimal(..)
  , earthAnimalSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthAnimal = EarthAnimal Bool
  deriving (Show, Eq)

earthAnimalSchema :: FC.Fleece schema => schema EarthAnimal
earthAnimalSchema =
  FC.coerceSchema FC.boolean