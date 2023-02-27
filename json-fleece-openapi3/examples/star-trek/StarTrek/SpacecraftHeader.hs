{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftHeader
  ( SpacecraftHeader(..)
  , spacecraftHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.SpacecraftHeader.Name (Name, nameSchema)
import StarTrek.SpacecraftHeader.Uid (Uid, uidSchema)

data SpacecraftHeader = SpacecraftHeader
  { name :: Name -- ^ Spacecraft name
  , uid :: Uid -- ^ Spacecraft unique ID
  }
  deriving (Eq, Show)

spacecraftHeaderSchema :: FC.Fleece schema => schema SpacecraftHeader
spacecraftHeaderSchema =
  FC.object $
    FC.constructor SpacecraftHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema