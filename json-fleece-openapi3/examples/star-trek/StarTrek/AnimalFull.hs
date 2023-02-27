{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalFull
  ( AnimalFull(..)
  , animalFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AnimalFull.Avian (Avian, avianSchema)
import StarTrek.AnimalFull.Canine (Canine, canineSchema)
import StarTrek.AnimalFull.EarthAnimal (EarthAnimal, earthAnimalSchema)
import StarTrek.AnimalFull.EarthInsect (EarthInsect, earthInsectSchema)
import StarTrek.AnimalFull.Feline (Feline, felineSchema)
import StarTrek.AnimalFull.Name (Name, nameSchema)
import StarTrek.AnimalFull.Uid (Uid, uidSchema)

data AnimalFull = AnimalFull
  { earthAnimal :: Maybe EarthAnimal -- ^ Whether it's an earth animal
  , name :: Name -- ^ Animal name
  , uid :: Uid -- ^ Animal unique ID
  , canine :: Maybe Canine -- ^ Whether it's a canine
  , earthInsect :: Maybe EarthInsect -- ^ Whether it's an earth insect
  , avian :: Maybe Avian -- ^ Whether it's an avian
  , feline :: Maybe Feline -- ^ Whether it's a feline
  }
  deriving (Eq, Show)

animalFullSchema :: FC.Fleece schema => schema AnimalFull
animalFullSchema =
  FC.object $
    FC.constructor AnimalFull
      #+ FC.optional "earthAnimal" earthAnimal earthAnimalSchema
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "canine" canine canineSchema
      #+ FC.optional "earthInsect" earthInsect earthInsectSchema
      #+ FC.optional "avian" avian avianSchema
      #+ FC.optional "feline" feline felineSchema