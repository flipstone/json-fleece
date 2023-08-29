{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftFull
  ( SpacecraftFull(..)
  , spacecraftFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OrganizationBase as OrganizationBase
import qualified StarTrek.Types.SpacecraftClassBase as SpacecraftClassBase
import qualified StarTrek.Types.SpacecraftFull.DateStatus as DateStatus
import qualified StarTrek.Types.SpacecraftFull.Name as Name
import qualified StarTrek.Types.SpacecraftFull.Registry as Registry
import qualified StarTrek.Types.SpacecraftFull.Status as Status
import qualified StarTrek.Types.SpacecraftFull.Uid as Uid
import qualified StarTrek.Types.SpacecraftType as SpacecraftType

data SpacecraftFull = SpacecraftFull
  { registry :: Maybe Registry.Registry -- ^ Spacecraft registry
  , status :: Maybe Status.Status -- ^ Status of a spacecraft (in prime reality, if spacecraft was in more than one realities)
  , owner :: Maybe OrganizationBase.OrganizationBase -- ^ Base organization, returned in search results
  , uid :: Uid.Uid -- ^ Spacecraft unique ID
  , dateStatus :: Maybe DateStatus.DateStatus -- ^ Date the spacecraft status was last known
  , name :: Name.Name -- ^ Spacecraft name
  , spacecraftTypes :: Maybe [SpacecraftType.SpacecraftType] -- ^ Rating of video release, etc.
  , operator :: Maybe OrganizationBase.OrganizationBase -- ^ Base organization, returned in search results
  , spacecraftClass :: Maybe SpacecraftClassBase.SpacecraftClassBase -- ^ Base spacecraft class, returned in search results
  }
  deriving (Eq, Show)

spacecraftFullSchema :: FC.Fleece schema => schema SpacecraftFull
spacecraftFullSchema =
  FC.object $
    FC.constructor SpacecraftFull
      #+ FC.optional "registry" registry Registry.registrySchema
      #+ FC.optional "status" status Status.statusSchema
      #+ FC.optional "owner" owner OrganizationBase.organizationBaseSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "dateStatus" dateStatus DateStatus.dateStatusSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "spacecraftTypes" spacecraftTypes (FC.list SpacecraftType.spacecraftTypeSchema)
      #+ FC.optional "operator" operator OrganizationBase.organizationBaseSchema
      #+ FC.optional "spacecraftClass" spacecraftClass SpacecraftClassBase.spacecraftClassBaseSchema