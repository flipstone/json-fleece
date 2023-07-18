{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassHeader
  ( SpacecraftClassHeader(..)
  , spacecraftClassHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.SpacecraftClassHeader.Name as Name
import qualified StarTrek.Types.SpacecraftClassHeader.Uid as Uid

data SpacecraftClassHeader = SpacecraftClassHeader
  { uid :: Uid.Uid -- ^ Spacecraft class unique ID
  , name :: Name.Name -- ^ Spacecraft class name
  }
  deriving (Eq, Show)

spacecraftClassHeaderSchema :: FC.Fleece schema => schema SpacecraftClassHeader
spacecraftClassHeaderSchema =
  FC.object $
    FC.constructor SpacecraftClassHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema