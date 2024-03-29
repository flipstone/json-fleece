{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftBase
  ( SpacecraftBase(..)
  , spacecraftBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OrganizationHeader as OrganizationHeader
import qualified StarTrek.Types.SpacecraftBase.DateStatus as DateStatus
import qualified StarTrek.Types.SpacecraftBase.Name as Name
import qualified StarTrek.Types.SpacecraftBase.Registry as Registry
import qualified StarTrek.Types.SpacecraftBase.Status as Status
import qualified StarTrek.Types.SpacecraftBase.Uid as Uid
import qualified StarTrek.Types.SpacecraftClassHeader as SpacecraftClassHeader

data SpacecraftBase = SpacecraftBase
  { registry :: Maybe Registry.Registry -- ^ Spacecraft registry
  , status :: Maybe Status.Status -- ^ Status of a spacecraft (in prime reality, if spacecraft was in more than one realities)
  , owner :: Maybe OrganizationHeader.OrganizationHeader -- ^ Header organization, embedded in other objects
  , uid :: Uid.Uid -- ^ Spacecraft unique ID
  , dateStatus :: Maybe DateStatus.DateStatus -- ^ Date the spacecraft status was last known
  , name :: Name.Name -- ^ Spacecraft name
  , operator :: Maybe OrganizationHeader.OrganizationHeader -- ^ Header organization, embedded in other objects
  , spacecraftClass :: Maybe SpacecraftClassHeader.SpacecraftClassHeader -- ^ Header spacecraft class, embedded in other objects
  }
  deriving (Eq, Show)

spacecraftBaseSchema :: FC.Fleece schema => schema SpacecraftBase
spacecraftBaseSchema =
  FC.object $
    FC.constructor SpacecraftBase
      #+ FC.optional "registry" registry Registry.registrySchema
      #+ FC.optional "status" status Status.statusSchema
      #+ FC.optional "owner" owner OrganizationHeader.organizationHeaderSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "dateStatus" dateStatus DateStatus.dateStatusSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "operator" operator OrganizationHeader.organizationHeaderSchema
      #+ FC.optional "spacecraftClass" spacecraftClass SpacecraftClassHeader.spacecraftClassHeaderSchema