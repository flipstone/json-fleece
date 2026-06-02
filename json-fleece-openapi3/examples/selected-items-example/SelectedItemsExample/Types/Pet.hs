{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.Pet
  ( Pet(..)
  , petSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified SelectedItemsExample.Types.PetName as PetName
import qualified SelectedItemsExample.Types.PetTag as PetTag

data Pet = Pet
  { name :: PetName.PetName
  , tag :: PetTag.PetTag
  }
  deriving (Eq, Show)

petSchema :: FC.Fleece t => FC.Schema t Pet
petSchema =
  FC.object $
    FC.constructor Pet
      #+ FC.required "name" name PetName.petNameSchema
      #+ FC.required "tag" tag PetTag.petTagSchema