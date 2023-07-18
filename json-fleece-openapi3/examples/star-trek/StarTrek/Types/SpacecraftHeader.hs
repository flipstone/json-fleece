{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftHeader
  ( SpacecraftHeader(..)
  , spacecraftHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.SpacecraftHeader.Name as Name
import qualified StarTrek.Types.SpacecraftHeader.Uid as Uid

data SpacecraftHeader = SpacecraftHeader
  { uid :: Uid.Uid -- ^ Spacecraft unique ID
  , name :: Name.Name -- ^ Spacecraft name
  }
  deriving (Eq, Show)

spacecraftHeaderSchema :: FC.Fleece schema => schema SpacecraftHeader
spacecraftHeaderSchema =
  FC.object $
    FC.constructor SpacecraftHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema