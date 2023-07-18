{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalHeader
  ( AnimalHeader(..)
  , animalHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.AnimalHeader.Name as Name
import qualified StarTrek.Types.AnimalHeader.Uid as Uid

data AnimalHeader = AnimalHeader
  { uid :: Uid.Uid -- ^ Animal unique ID
  , name :: Name.Name -- ^ Animal name
  }
  deriving (Eq, Show)

animalHeaderSchema :: FC.Fleece schema => schema AnimalHeader
animalHeaderSchema =
  FC.object $
    FC.constructor AnimalHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema