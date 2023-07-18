{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftType
  ( SpacecraftType(..)
  , spacecraftTypeSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SpacecraftType.Name as Name
import qualified StarTrek.Types.SpacecraftType.Uid as Uid

data SpacecraftType = SpacecraftType
  { uid :: Maybe Uid.Uid -- ^ Spacecraft type unique ID
  , name :: Maybe Name.Name -- ^ Spacecraft type name
  }
  deriving (Eq, Show)

spacecraftTypeSchema :: FC.Fleece schema => schema SpacecraftType
spacecraftTypeSchema =
  FC.object $
    FC.constructor SpacecraftType
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "name" name Name.nameSchema