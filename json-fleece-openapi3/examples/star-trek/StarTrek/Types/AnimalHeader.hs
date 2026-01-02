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
  { name :: Name.Name -- ^ Animal name
  , uid :: Uid.Uid -- ^ Animal unique ID
  }
  deriving (Eq, Show)

animalHeaderSchema :: FC.Fleece t => FC.Schema t AnimalHeader
animalHeaderSchema =
  FC.object $
    FC.constructor AnimalHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema