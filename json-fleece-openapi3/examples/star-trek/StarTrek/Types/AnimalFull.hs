{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalFull
  ( AnimalFull(..)
  , animalFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AnimalFull.Avian as Avian
import qualified StarTrek.Types.AnimalFull.Canine as Canine
import qualified StarTrek.Types.AnimalFull.EarthAnimal as EarthAnimal
import qualified StarTrek.Types.AnimalFull.EarthInsect as EarthInsect
import qualified StarTrek.Types.AnimalFull.Feline as Feline
import qualified StarTrek.Types.AnimalFull.Name as Name
import qualified StarTrek.Types.AnimalFull.Uid as Uid

data AnimalFull = AnimalFull
  { feline :: Maybe Feline.Feline -- ^ Whether it's a feline
  , avian :: Maybe Avian.Avian -- ^ Whether it's an avian
  , earthInsect :: Maybe EarthInsect.EarthInsect -- ^ Whether it's an earth insect
  , uid :: Uid.Uid -- ^ Animal unique ID
  , name :: Name.Name -- ^ Animal name
  , earthAnimal :: Maybe EarthAnimal.EarthAnimal -- ^ Whether it's an earth animal
  , canine :: Maybe Canine.Canine -- ^ Whether it's a canine
  }
  deriving (Eq, Show)

animalFullSchema :: FC.Fleece schema => schema AnimalFull
animalFullSchema =
  FC.object $
    FC.constructor AnimalFull
      #+ FC.optional "feline" feline Feline.felineSchema
      #+ FC.optional "avian" avian Avian.avianSchema
      #+ FC.optional "earthInsect" earthInsect EarthInsect.earthInsectSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "earthAnimal" earthAnimal EarthAnimal.earthAnimalSchema
      #+ FC.optional "canine" canine Canine.canineSchema