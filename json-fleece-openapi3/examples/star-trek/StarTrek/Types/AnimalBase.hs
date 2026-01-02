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
  { avian :: Maybe Avian.Avian -- ^ Whether it's an avian
  , canine :: Maybe Canine.Canine -- ^ Whether it's a canine
  , earthAnimal :: Maybe EarthAnimal.EarthAnimal -- ^ Whether it's an earth animal
  , earthInsect :: Maybe EarthInsect.EarthInsect -- ^ Whether it's an earth insect
  , feline :: Maybe Feline.Feline -- ^ Whether it's a feline
  , name :: Name.Name -- ^ Animal name
  , uid :: Uid.Uid -- ^ Animal unique ID
  }
  deriving (Eq, Show)

animalBaseSchema :: FC.Fleece t => FC.Schema t AnimalBase
animalBaseSchema =
  FC.object $
    FC.constructor AnimalBase
      #+ FC.optional "avian" avian Avian.avianSchema
      #+ FC.optional "canine" canine Canine.canineSchema
      #+ FC.optional "earthAnimal" earthAnimal EarthAnimal.earthAnimalSchema
      #+ FC.optional "earthInsect" earthInsect EarthInsect.earthInsectSchema
      #+ FC.optional "feline" feline Feline.felineSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema