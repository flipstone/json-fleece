{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBase
  ( OrganizationBase(..)
  , organizationBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data OrganizationBase = OrganizationBase
  { alternateReality :: Maybe Bool -- ^ Whether this location is from alternate reality
  , name :: Text -- ^ Organization name
  , militaryOrganization :: Maybe Bool -- ^ Whether it's a military organization
  , prisonOrPenalColony :: Maybe Bool -- ^ Whether it's a prison or penal colony
  , intergovernmentalOrganization :: Maybe Bool -- ^ Whether it's an intergovernmental organization
  , uid :: Text -- ^ Organization unique ID
  , governmentAgency :: Maybe Bool -- ^ Whether it's a government agency
  , mirror :: Maybe Bool -- ^ Whether this organization is from mirror universe
  , militaryUnit :: Maybe Bool -- ^ Whether it's a military unit
  , researchOrganization :: Maybe Bool -- ^ Whether it's a research organization
  , government :: Maybe Bool -- ^ Whether it's a government
  , lawEnforcementAgency :: Maybe Bool -- ^ Whether it's a law enforcement agency
  , sportOrganization :: Maybe Bool -- ^ Whether it's a sport organization
  , medicalOrganization :: Maybe Bool -- ^ Whether it's a medical organization
  }
  deriving (Eq, Show)

organizationBaseSchema :: FC.Fleece schema => schema OrganizationBase
organizationBaseSchema =
  FC.object $
    FC.constructor OrganizationBase
      #+ FC.optionalField FC.OmitKey_DelegateNull "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "militaryOrganization" militaryOrganization FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "prisonOrPenalColony" prisonOrPenalColony FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "intergovernmentalOrganization" intergovernmentalOrganization FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "governmentAgency" governmentAgency FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "mirror" mirror FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "militaryUnit" militaryUnit FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "researchOrganization" researchOrganization FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "government" government FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "lawEnforcementAgency" lawEnforcementAgency FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "sportOrganization" sportOrganization FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "medicalOrganization" medicalOrganization FC.boolean