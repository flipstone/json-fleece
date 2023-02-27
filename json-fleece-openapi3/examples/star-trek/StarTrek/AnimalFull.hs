{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalFull
  ( AnimalFull(..)
  , animalFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data AnimalFull = AnimalFull
  { earthAnimal :: Maybe Bool -- ^ Whether it's an earth animal
  , name :: Text -- ^ Animal name
  , uid :: Text -- ^ Animal unique ID
  , canine :: Maybe Bool -- ^ Whether it's a canine
  , earthInsect :: Maybe Bool -- ^ Whether it's an earth insect
  , avian :: Maybe Bool -- ^ Whether it's an avian
  , feline :: Maybe Bool -- ^ Whether it's a feline
  }
  deriving (Eq, Show)

animalFullSchema :: FC.Fleece schema => schema AnimalFull
animalFullSchema =
  FC.object $
    FC.constructor AnimalFull
      #+ FC.optional "earthAnimal" earthAnimal FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "canine" canine FC.boolean
      #+ FC.optional "earthInsect" earthInsect FC.boolean
      #+ FC.optional "avian" avian FC.boolean
      #+ FC.optional "feline" feline FC.boolean