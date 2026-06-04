{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.NewPet
  ( NewPet(..)
  , newPetSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified SelectedItemsExample.Types.PetName as PetName

newtype NewPet = NewPet
  { name :: PetName.PetName
  }
  deriving (Eq, Show)

newPetSchema :: FC.Fleece t => FC.Schema t NewPet
newPetSchema =
  FC.object $
    FC.constructor NewPet
      #+ FC.required "name" name PetName.petNameSchema