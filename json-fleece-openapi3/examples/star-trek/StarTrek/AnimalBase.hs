{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBase
  ( AnimalBase(..)
  , animalBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.AnimalBase.Avian as Avian
import qualified StarTrek.AnimalBase.Canine as Canine
import qualified StarTrek.AnimalBase.EarthAnimal as EarthAnimal
import qualified StarTrek.AnimalBase.EarthInsect as EarthInsect
import qualified StarTrek.AnimalBase.Feline as Feline
import qualified StarTrek.AnimalBase.Name as Name
import qualified StarTrek.AnimalBase.Uid as Uid

data AnimalBase = AnimalBase
  { earthAnimal :: Maybe EarthAnimal.EarthAnimal -- ^ Whether it's an earth animal
  , name :: Name.Name -- ^ Animal name
  , uid :: Uid.Uid -- ^ Animal unique ID
  , canine :: Maybe Canine.Canine -- ^ Whether it's a canine
  , earthInsect :: Maybe EarthInsect.EarthInsect -- ^ Whether it's an earth insect
  , avian :: Maybe Avian.Avian -- ^ Whether it's an avian
  , feline :: Maybe Feline.Feline -- ^ Whether it's a feline
  }
  deriving (Eq, Show)

animalBaseSchema :: FC.Fleece schema => schema AnimalBase
animalBaseSchema =
  FC.object $
    FC.constructor AnimalBase
      #+ FC.optional "earthAnimal" earthAnimal EarthAnimal.earthAnimalSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "canine" canine Canine.canineSchema
      #+ FC.optional "earthInsect" earthInsect EarthInsect.earthInsectSchema
      #+ FC.optional "avian" avian Avian.avianSchema
      #+ FC.optional "feline" feline Feline.felineSchema