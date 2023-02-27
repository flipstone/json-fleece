{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationFull
  ( OrganizationFull(..)
  , organizationFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)

data OrganizationFull = OrganizationFull
  { alternateReality :: Maybe Bool -- ^ Whether this organization is from alternate reality
  , name :: Text -- ^ Organization name
  , militaryOrganization :: Maybe Bool -- ^ Whether it's a military organization
  , prisonOrPenalColony :: Maybe Bool -- ^ Whether it's a prison or penal colony
  , intergovernmentalOrganization :: Maybe Bool -- ^ Whether it's an intergovernmental organization
  , uid :: Text -- ^ Organization unique ID
  , governmentAgency :: Maybe Bool -- ^ Whether it's a government agency
  , mirror :: Maybe Bool -- ^ Whether this organization is from mirror universe
  , characters :: Maybe [CharacterBase] -- ^ Characters belonging to this organization
  , militaryUnit :: Maybe Bool -- ^ Whether it's a military unit
  , researchOrganization :: Maybe Bool -- ^ Whether it's a research organization
  , government :: Maybe Bool -- ^ Whether it's a government
  , lawEnforcementAgency :: Maybe Bool -- ^ Whether it's a law enforcement agency
  , sportOrganization :: Maybe Bool -- ^ Whether it's a sport organization
  , medicalOrganization :: Maybe Bool -- ^ Whether it's a medical organization
  }
  deriving (Eq, Show)

organizationFullSchema :: FC.Fleece schema => schema OrganizationFull
organizationFullSchema =
  FC.object $
    FC.constructor OrganizationFull
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "militaryOrganization" militaryOrganization FC.boolean
      #+ FC.optional "prisonOrPenalColony" prisonOrPenalColony FC.boolean
      #+ FC.optional "intergovernmentalOrganization" intergovernmentalOrganization FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "governmentAgency" governmentAgency FC.boolean
      #+ FC.optional "mirror" mirror FC.boolean
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "militaryUnit" militaryUnit FC.boolean
      #+ FC.optional "researchOrganization" researchOrganization FC.boolean
      #+ FC.optional "government" government FC.boolean
      #+ FC.optional "lawEnforcementAgency" lawEnforcementAgency FC.boolean
      #+ FC.optional "sportOrganization" sportOrganization FC.boolean
      #+ FC.optional "medicalOrganization" medicalOrganization FC.boolean