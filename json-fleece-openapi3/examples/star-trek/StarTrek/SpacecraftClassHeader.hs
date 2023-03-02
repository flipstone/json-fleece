{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassHeader
  ( SpacecraftClassHeader(..)
  , spacecraftClassHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.SpacecraftClassHeader.Name as Name
import qualified StarTrek.SpacecraftClassHeader.Uid as Uid

data SpacecraftClassHeader = SpacecraftClassHeader
  { name :: Name.Name -- ^ Spacecraft class name
  , uid :: Uid.Uid -- ^ Spacecraft class unique ID
  }
  deriving (Eq, Show)

spacecraftClassHeaderSchema :: FC.Fleece schema => schema SpacecraftClassHeader
spacecraftClassHeaderSchema =
  FC.object $
    FC.constructor SpacecraftClassHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema