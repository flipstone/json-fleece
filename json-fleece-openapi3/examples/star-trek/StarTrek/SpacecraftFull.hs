{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftFull
  ( SpacecraftFull(..)
  , spacecraftFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.OrganizationBase as OrganizationBase
import qualified StarTrek.SpacecraftClassBase as SpacecraftClassBase
import qualified StarTrek.SpacecraftFull.DateStatus as DateStatus
import qualified StarTrek.SpacecraftFull.Name as Name
import qualified StarTrek.SpacecraftFull.Registry as Registry
import qualified StarTrek.SpacecraftFull.Status as Status
import qualified StarTrek.SpacecraftFull.Uid as Uid
import qualified StarTrek.SpacecraftType as SpacecraftType

data SpacecraftFull = SpacecraftFull
  { name :: Name.Name -- ^ Spacecraft name
  , registry :: Maybe Registry.Registry -- ^ Spacecraft registry
  , uid :: Uid.Uid -- ^ Spacecraft unique ID
  , status :: Maybe Status.Status -- ^ Status of a spacecraft (in prime reality, if spacecraft was in more than one realities)
  , owner :: Maybe OrganizationBase.OrganizationBase -- ^ Base organization, returned in search results
  , operator :: Maybe OrganizationBase.OrganizationBase -- ^ Base organization, returned in search results
  , spacecraftClass :: Maybe SpacecraftClassBase.SpacecraftClassBase -- ^ Base spacecraft class, returned in search results
  , dateStatus :: Maybe DateStatus.DateStatus -- ^ Date the spacecraft status was last known
  , spacecraftTypes :: Maybe [SpacecraftType.SpacecraftType] -- ^ Rating of video release, etc.
  }
  deriving (Eq, Show)

spacecraftFullSchema :: FC.Fleece schema => schema SpacecraftFull
spacecraftFullSchema =
  FC.object $
    FC.constructor SpacecraftFull
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "registry" registry Registry.registrySchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "status" status Status.statusSchema
      #+ FC.optional "owner" owner OrganizationBase.organizationBaseSchema
      #+ FC.optional "operator" operator OrganizationBase.organizationBaseSchema
      #+ FC.optional "spacecraftClass" spacecraftClass SpacecraftClassBase.spacecraftClassBaseSchema
      #+ FC.optional "dateStatus" dateStatus DateStatus.dateStatusSchema
      #+ FC.optional "spacecraftTypes" spacecraftTypes (FC.list SpacecraftType.spacecraftTypeSchema)