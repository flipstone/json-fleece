{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftFull
  ( SpacecraftFull(..)
  , spacecraftFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)
import StarTrek.SpacecraftClassBase (SpacecraftClassBase, spacecraftClassBaseSchema)
import StarTrek.SpacecraftFull.DateStatus (DateStatus, dateStatusSchema)
import StarTrek.SpacecraftFull.Name (Name, nameSchema)
import StarTrek.SpacecraftFull.Registry (Registry, registrySchema)
import StarTrek.SpacecraftFull.Status (Status, statusSchema)
import StarTrek.SpacecraftFull.Uid (Uid, uidSchema)
import StarTrek.SpacecraftType (SpacecraftType, spacecraftTypeSchema)

data SpacecraftFull = SpacecraftFull
  { name :: Name -- ^ Spacecraft name
  , registry :: Maybe Registry -- ^ Spacecraft registry
  , uid :: Uid -- ^ Spacecraft unique ID
  , status :: Maybe Status -- ^ Status of a spacecraft (in prime reality, if spacecraft was in more than one realities)
  , owner :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , operator :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , spacecraftClass :: Maybe SpacecraftClassBase -- ^ Base spacecraft class, returned in search results
  , dateStatus :: Maybe DateStatus -- ^ Date the spacecraft status was last known
  , spacecraftTypes :: Maybe [SpacecraftType] -- ^ Rating of video release, etc.
  }
  deriving (Eq, Show)

spacecraftFullSchema :: FC.Fleece schema => schema SpacecraftFull
spacecraftFullSchema =
  FC.object $
    FC.constructor SpacecraftFull
      #+ FC.required "name" name nameSchema
      #+ FC.optional "registry" registry registrySchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "status" status statusSchema
      #+ FC.optional "owner" owner organizationBaseSchema
      #+ FC.optional "operator" operator organizationBaseSchema
      #+ FC.optional "spacecraftClass" spacecraftClass spacecraftClassBaseSchema
      #+ FC.optional "dateStatus" dateStatus dateStatusSchema
      #+ FC.optional "spacecraftTypes" spacecraftTypes (FC.list spacecraftTypeSchema)