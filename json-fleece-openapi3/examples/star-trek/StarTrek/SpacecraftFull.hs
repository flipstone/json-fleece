{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftFull
  ( SpacecraftFull(..)
  , spacecraftFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)
import StarTrek.SpacecraftClassBase (SpacecraftClassBase, spacecraftClassBaseSchema)
import StarTrek.SpacecraftType (SpacecraftType, spacecraftTypeSchema)

data SpacecraftFull = SpacecraftFull
  { name :: Text -- ^ Spacecraft name
  , registry :: Maybe Text -- ^ Spacecraft registry
  , uid :: Text -- ^ Spacecraft unique ID
  , status :: Maybe Text -- ^ Status of a spacecraft (in prime reality, if spacecraft was in more than one realities)
  , owner :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , operator :: Maybe OrganizationBase -- ^ Base organization, returned in search results
  , spacecraftClass :: Maybe SpacecraftClassBase -- ^ Base spacecraft class, returned in search results
  , dateStatus :: Maybe Text -- ^ Date the spacecraft status was last known
  , spacecraftTypes :: Maybe [SpacecraftType] -- ^ Spacecraft types
  }
  deriving (Eq, Show)

spacecraftFullSchema :: FC.Fleece schema => schema SpacecraftFull
spacecraftFullSchema =
  FC.object $
    FC.constructor SpacecraftFull
      #+ FC.required "name" name FC.text
      #+ FC.optional "registry" registry FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "status" status FC.text
      #+ FC.optional "owner" owner organizationBaseSchema
      #+ FC.optional "operator" operator organizationBaseSchema
      #+ FC.optional "spacecraftClass" spacecraftClass spacecraftClassBaseSchema
      #+ FC.optional "dateStatus" dateStatus FC.text
      #+ FC.optional "spacecraftTypes" spacecraftTypes (FC.list spacecraftTypeSchema)