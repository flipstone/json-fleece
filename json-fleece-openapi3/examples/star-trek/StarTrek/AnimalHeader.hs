{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalHeader
  ( AnimalHeader(..)
  , animalHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.AnimalHeader.Name (Name, nameSchema)
import StarTrek.AnimalHeader.Uid (Uid, uidSchema)

data AnimalHeader = AnimalHeader
  { name :: Name -- ^ Animal name
  , uid :: Uid -- ^ Animal unique ID
  }
  deriving (Eq, Show)

animalHeaderSchema :: FC.Fleece schema => schema AnimalHeader
animalHeaderSchema =
  FC.object $
    FC.constructor AnimalHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema