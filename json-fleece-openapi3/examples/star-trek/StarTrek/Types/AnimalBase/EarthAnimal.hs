{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalBase.EarthAnimal
  ( EarthAnimal(..)
  , earthAnimalSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthAnimal = EarthAnimal Bool
  deriving (Show, Eq)

earthAnimalSchema :: FC.Fleece t => FC.Schema t EarthAnimal
earthAnimalSchema =
  FC.coerceSchema FC.boolean