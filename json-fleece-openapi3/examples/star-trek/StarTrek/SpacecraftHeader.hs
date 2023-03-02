{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftHeader
  ( SpacecraftHeader(..)
  , spacecraftHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.SpacecraftHeader.Name as Name
import qualified StarTrek.SpacecraftHeader.Uid as Uid

data SpacecraftHeader = SpacecraftHeader
  { name :: Name.Name -- ^ Spacecraft name
  , uid :: Uid.Uid -- ^ Spacecraft unique ID
  }
  deriving (Eq, Show)

spacecraftHeaderSchema :: FC.Fleece schema => schema SpacecraftHeader
spacecraftHeaderSchema =
  FC.object $
    FC.constructor SpacecraftHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema