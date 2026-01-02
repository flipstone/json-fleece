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
  { name :: Maybe Name.Name -- ^ Spacecraft type name
  , uid :: Maybe Uid.Uid -- ^ Spacecraft type unique ID
  }
  deriving (Eq, Show)

spacecraftTypeSchema :: FC.Fleece t => FC.Schema t SpacecraftType
spacecraftTypeSchema =
  FC.object $
    FC.constructor SpacecraftType
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "uid" uid Uid.uidSchema