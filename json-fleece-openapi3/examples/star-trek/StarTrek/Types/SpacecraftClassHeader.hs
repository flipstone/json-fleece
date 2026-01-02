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
  { name :: Name.Name -- ^ Spacecraft class name
  , uid :: Uid.Uid -- ^ Spacecraft class unique ID
  }
  deriving (Eq, Show)

spacecraftClassHeaderSchema :: FC.Fleece t => FC.Schema t SpacecraftClassHeader
spacecraftClassHeaderSchema =
  FC.object $
    FC.constructor SpacecraftClassHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema