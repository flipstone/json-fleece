{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftType
  ( SpacecraftType(..)
  , spacecraftTypeSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SpacecraftType.Name (Name, nameSchema)
import StarTrek.SpacecraftType.Uid (Uid, uidSchema)

data SpacecraftType = SpacecraftType
  { name :: Maybe Name -- ^ Spacecraft type name
  , uid :: Maybe Uid -- ^ Spacecraft type unique ID
  }
  deriving (Eq, Show)

spacecraftTypeSchema :: FC.Fleece schema => schema SpacecraftType
spacecraftTypeSchema =
  FC.object $
    FC.constructor SpacecraftType
      #+ FC.optional "name" name nameSchema
      #+ FC.optional "uid" uid uidSchema