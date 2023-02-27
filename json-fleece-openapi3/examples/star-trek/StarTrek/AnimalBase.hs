{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBase
  ( AnimalBase(..)
  , animalBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AnimalBase.Avian (Avian, avianSchema)
import StarTrek.AnimalBase.Canine (Canine, canineSchema)
import StarTrek.AnimalBase.EarthAnimal (EarthAnimal, earthAnimalSchema)
import StarTrek.AnimalBase.EarthInsect (EarthInsect, earthInsectSchema)
import StarTrek.AnimalBase.Feline (Feline, felineSchema)
import StarTrek.AnimalBase.Name (Name, nameSchema)
import StarTrek.AnimalBase.Uid (Uid, uidSchema)

data AnimalBase = AnimalBase
  { earthAnimal :: Maybe EarthAnimal -- ^ Whether it's an earth animal
  , name :: Name -- ^ Animal name
  , uid :: Uid -- ^ Animal unique ID
  , canine :: Maybe Canine -- ^ Whether it's a canine
  , earthInsect :: Maybe EarthInsect -- ^ Whether it's an earth insect
  , avian :: Maybe Avian -- ^ Whether it's an avian
  , feline :: Maybe Feline -- ^ Whether it's a feline
  }
  deriving (Eq, Show)

animalBaseSchema :: FC.Fleece schema => schema AnimalBase
animalBaseSchema =
  FC.object $
    FC.constructor AnimalBase
      #+ FC.optional "earthAnimal" earthAnimal earthAnimalSchema
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "canine" canine canineSchema
      #+ FC.optional "earthInsect" earthInsect earthInsectSchema
      #+ FC.optional "avian" avian avianSchema
      #+ FC.optional "feline" feline felineSchema