{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBase
  ( OrganizationBase(..)
  , organizationBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OrganizationBase.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.OrganizationBase.Government (Government, governmentSchema)
import StarTrek.OrganizationBase.GovernmentAgency (GovernmentAgency, governmentAgencySchema)
import StarTrek.OrganizationBase.IntergovernmentalOrganization (IntergovernmentalOrganization, intergovernmentalOrganizationSchema)
import StarTrek.OrganizationBase.LawEnforcementAgency (LawEnforcementAgency, lawEnforcementAgencySchema)
import StarTrek.OrganizationBase.MedicalOrganization (MedicalOrganization, medicalOrganizationSchema)
import StarTrek.OrganizationBase.MilitaryOrganization (MilitaryOrganization, militaryOrganizationSchema)
import StarTrek.OrganizationBase.MilitaryUnit (MilitaryUnit, militaryUnitSchema)
import StarTrek.OrganizationBase.Mirror (Mirror, mirrorSchema)
import StarTrek.OrganizationBase.Name (Name, nameSchema)
import StarTrek.OrganizationBase.PrisonOrPenalColony (PrisonOrPenalColony, prisonOrPenalColonySchema)
import StarTrek.OrganizationBase.ResearchOrganization (ResearchOrganization, researchOrganizationSchema)
import StarTrek.OrganizationBase.SportOrganization (SportOrganization, sportOrganizationSchema)
import StarTrek.OrganizationBase.Uid (Uid, uidSchema)

data OrganizationBase = OrganizationBase
  { alternateReality :: Maybe AlternateReality -- ^ Whether this location is from alternate reality
  , name :: Name -- ^ Organization name
  , militaryOrganization :: Maybe MilitaryOrganization -- ^ Whether it's a military organization
  , prisonOrPenalColony :: Maybe PrisonOrPenalColony -- ^ Whether it's a prison or penal colony
  , intergovernmentalOrganization :: Maybe IntergovernmentalOrganization -- ^ Whether it's an intergovernmental organization
  , uid :: Uid -- ^ Organization unique ID
  , governmentAgency :: Maybe GovernmentAgency -- ^ Whether it's a government agency
  , mirror :: Maybe Mirror -- ^ Whether this organization is from mirror universe
  , militaryUnit :: Maybe MilitaryUnit -- ^ Whether it's a military unit
  , researchOrganization :: Maybe ResearchOrganization -- ^ Whether it's a research organization
  , government :: Maybe Government -- ^ Whether it's a government
  , lawEnforcementAgency :: Maybe LawEnforcementAgency -- ^ Whether it's a law enforcement agency
  , sportOrganization :: Maybe SportOrganization -- ^ Whether it's a sport organization
  , medicalOrganization :: Maybe MedicalOrganization -- ^ Whether it's a medical organization
  }
  deriving (Eq, Show)

organizationBaseSchema :: FC.Fleece schema => schema OrganizationBase
organizationBaseSchema =
  FC.object $
    FC.constructor OrganizationBase
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "militaryOrganization" militaryOrganization militaryOrganizationSchema
      #+ FC.optional "prisonOrPenalColony" prisonOrPenalColony prisonOrPenalColonySchema
      #+ FC.optional "intergovernmentalOrganization" intergovernmentalOrganization intergovernmentalOrganizationSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "governmentAgency" governmentAgency governmentAgencySchema
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "militaryUnit" militaryUnit militaryUnitSchema
      #+ FC.optional "researchOrganization" researchOrganization researchOrganizationSchema
      #+ FC.optional "government" government governmentSchema
      #+ FC.optional "lawEnforcementAgency" lawEnforcementAgency lawEnforcementAgencySchema
      #+ FC.optional "sportOrganization" sportOrganization sportOrganizationSchema
      #+ FC.optional "medicalOrganization" medicalOrganization medicalOrganizationSchema