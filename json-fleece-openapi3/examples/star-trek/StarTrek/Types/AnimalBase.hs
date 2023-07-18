{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalBase
  ( AnimalBase(..)
  , animalBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AnimalBase.Avian as Avian
import qualified StarTrek.Types.AnimalBase.Canine as Canine
import qualified StarTrek.Types.AnimalBase.EarthAnimal as EarthAnimal
import qualified StarTrek.Types.AnimalBase.EarthInsect as EarthInsect
import qualified StarTrek.Types.AnimalBase.Feline as Feline
import qualified StarTrek.Types.AnimalBase.Name as Name
import qualified StarTrek.Types.AnimalBase.Uid as Uid

data AnimalBase = AnimalBase
  { canine :: Maybe Canine.Canine -- ^ Whether it's a canine
  , feline :: Maybe Feline.Feline -- ^ Whether it's a feline
  , avian :: Maybe Avian.Avian -- ^ Whether it's an avian
  , earthAnimal :: Maybe EarthAnimal.EarthAnimal -- ^ Whether it's an earth animal
  , uid :: Uid.Uid -- ^ Animal unique ID
  , earthInsect :: Maybe EarthInsect.EarthInsect -- ^ Whether it's an earth insect
  , name :: Name.Name -- ^ Animal name
  }
  deriving (Eq, Show)

animalBaseSchema :: FC.Fleece schema => schema AnimalBase
animalBaseSchema =
  FC.object $
    FC.constructor AnimalBase
      #+ FC.optional "canine" canine Canine.canineSchema
      #+ FC.optional "feline" feline Feline.felineSchema
      #+ FC.optional "avian" avian Avian.avianSchema
      #+ FC.optional "earthAnimal" earthAnimal EarthAnimal.earthAnimalSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "earthInsect" earthInsect EarthInsect.earthInsectSchema
      #+ FC.required "name" name Name.nameSchema