{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalHeader
  ( AnimalHeader(..)
  , animalHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.AnimalHeader.Name as Name
import qualified StarTrek.AnimalHeader.Uid as Uid

data AnimalHeader = AnimalHeader
  { name :: Name.Name -- ^ Animal name
  , uid :: Uid.Uid -- ^ Animal unique ID
  }
  deriving (Eq, Show)

animalHeaderSchema :: FC.Fleece schema => schema AnimalHeader
animalHeaderSchema =
  FC.object $
    FC.constructor AnimalHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema