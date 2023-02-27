{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBase
  ( AnimalBase(..)
  , animalBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data AnimalBase = AnimalBase
  { earthAnimal :: Maybe Bool -- ^ Whether it's an earth animal
  , name :: Text -- ^ Animal name
  , uid :: Text -- ^ Animal unique ID
  , canine :: Maybe Bool -- ^ Whether it's a canine
  , earthInsect :: Maybe Bool -- ^ Whether it's an earth insect
  , avian :: Maybe Bool -- ^ Whether it's an avian
  , feline :: Maybe Bool -- ^ Whether it's a feline
  }
  deriving (Eq, Show)

animalBaseSchema :: FC.Fleece schema => schema AnimalBase
animalBaseSchema =
  FC.object $
    FC.constructor AnimalBase
      #+ FC.optional "earthAnimal" earthAnimal FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "canine" canine FC.boolean
      #+ FC.optional "earthInsect" earthInsect FC.boolean
      #+ FC.optional "avian" avian FC.boolean
      #+ FC.optional "feline" feline FC.boolean