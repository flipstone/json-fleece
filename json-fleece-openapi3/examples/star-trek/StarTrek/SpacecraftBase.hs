{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftBase
  ( SpacecraftBase(..)
  , spacecraftBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationHeader (OrganizationHeader, organizationHeaderSchema)
import StarTrek.SpacecraftBase.DateStatus (DateStatus, dateStatusSchema)
import StarTrek.SpacecraftBase.Name (Name, nameSchema)
import StarTrek.SpacecraftBase.Registry (Registry, registrySchema)
import StarTrek.SpacecraftBase.Status (Status, statusSchema)
import StarTrek.SpacecraftBase.Uid (Uid, uidSchema)
import StarTrek.SpacecraftClassHeader (SpacecraftClassHeader, spacecraftClassHeaderSchema)

data SpacecraftBase = SpacecraftBase
  { name :: Name -- ^ Spacecraft name
  , registry :: Maybe Registry -- ^ Spacecraft registry
  , uid :: Uid -- ^ Spacecraft unique ID
  , status :: Maybe Status -- ^ Status of a spacecraft (in prime reality, if spacecraft was in more than one realities)
  , owner :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  , operator :: Maybe OrganizationHeader -- ^ Header organization, embedded in other objects
  , spacecraftClass :: Maybe SpacecraftClassHeader -- ^ Header spacecraft class, embedded in other objects
  , dateStatus :: Maybe DateStatus -- ^ Date the spacecraft status was last known
  }
  deriving (Eq, Show)

spacecraftBaseSchema :: FC.Fleece schema => schema SpacecraftBase
spacecraftBaseSchema =
  FC.object $
    FC.constructor SpacecraftBase
      #+ FC.required "name" name nameSchema
      #+ FC.optional "registry" registry registrySchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "status" status statusSchema
      #+ FC.optional "owner" owner organizationHeaderSchema
      #+ FC.optional "operator" operator organizationHeaderSchema
      #+ FC.optional "spacecraftClass" spacecraftClass spacecraftClassHeaderSchema
      #+ FC.optional "dateStatus" dateStatus dateStatusSchema