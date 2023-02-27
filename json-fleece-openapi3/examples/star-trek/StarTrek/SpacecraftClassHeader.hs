{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassHeader
  ( SpacecraftClassHeader(..)
  , spacecraftClassHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.SpacecraftClassHeader.Name (Name, nameSchema)
import StarTrek.SpacecraftClassHeader.Uid (Uid, uidSchema)

data SpacecraftClassHeader = SpacecraftClassHeader
  { name :: Name -- ^ Spacecraft class name
  , uid :: Uid -- ^ Spacecraft class unique ID
  }
  deriving (Eq, Show)

spacecraftClassHeaderSchema :: FC.Fleece schema => schema SpacecraftClassHeader
spacecraftClassHeaderSchema =
  FC.object $
    FC.constructor SpacecraftClassHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema