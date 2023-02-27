{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationFull
  ( OrganizationFull(..)
  , organizationFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.OrganizationFull.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.OrganizationFull.Government (Government, governmentSchema)
import StarTrek.OrganizationFull.GovernmentAgency (GovernmentAgency, governmentAgencySchema)
import StarTrek.OrganizationFull.IntergovernmentalOrganization (IntergovernmentalOrganization, intergovernmentalOrganizationSchema)
import StarTrek.OrganizationFull.LawEnforcementAgency (LawEnforcementAgency, lawEnforcementAgencySchema)
import StarTrek.OrganizationFull.MedicalOrganization (MedicalOrganization, medicalOrganizationSchema)
import StarTrek.OrganizationFull.MilitaryOrganization (MilitaryOrganization, militaryOrganizationSchema)
import StarTrek.OrganizationFull.MilitaryUnit (MilitaryUnit, militaryUnitSchema)
import StarTrek.OrganizationFull.Mirror (Mirror, mirrorSchema)
import StarTrek.OrganizationFull.Name (Name, nameSchema)
import StarTrek.OrganizationFull.PrisonOrPenalColony (PrisonOrPenalColony, prisonOrPenalColonySchema)
import StarTrek.OrganizationFull.ResearchOrganization (ResearchOrganization, researchOrganizationSchema)
import StarTrek.OrganizationFull.SportOrganization (SportOrganization, sportOrganizationSchema)
import StarTrek.OrganizationFull.Uid (Uid, uidSchema)

data OrganizationFull = OrganizationFull
  { alternateReality :: Maybe AlternateReality -- ^ Whether this organization is from alternate reality
  , name :: Name -- ^ Organization name
  , militaryOrganization :: Maybe MilitaryOrganization -- ^ Whether it's a military organization
  , prisonOrPenalColony :: Maybe PrisonOrPenalColony -- ^ Whether it's a prison or penal colony
  , intergovernmentalOrganization :: Maybe IntergovernmentalOrganization -- ^ Whether it's an intergovernmental organization
  , uid :: Uid -- ^ Organization unique ID
  , governmentAgency :: Maybe GovernmentAgency -- ^ Whether it's a government agency
  , mirror :: Maybe Mirror -- ^ Whether this organization is from mirror universe
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , militaryUnit :: Maybe MilitaryUnit -- ^ Whether it's a military unit
  , researchOrganization :: Maybe ResearchOrganization -- ^ Whether it's a research organization
  , government :: Maybe Government -- ^ Whether it's a government
  , lawEnforcementAgency :: Maybe LawEnforcementAgency -- ^ Whether it's a law enforcement agency
  , sportOrganization :: Maybe SportOrganization -- ^ Whether it's a sport organization
  , medicalOrganization :: Maybe MedicalOrganization -- ^ Whether it's a medical organization
  }
  deriving (Eq, Show)

organizationFullSchema :: FC.Fleece schema => schema OrganizationFull
organizationFullSchema =
  FC.object $
    FC.constructor OrganizationFull
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "militaryOrganization" militaryOrganization militaryOrganizationSchema
      #+ FC.optional "prisonOrPenalColony" prisonOrPenalColony prisonOrPenalColonySchema
      #+ FC.optional "intergovernmentalOrganization" intergovernmentalOrganization intergovernmentalOrganizationSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "governmentAgency" governmentAgency governmentAgencySchema
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "militaryUnit" militaryUnit militaryUnitSchema
      #+ FC.optional "researchOrganization" researchOrganization researchOrganizationSchema
      #+ FC.optional "government" government governmentSchema
      #+ FC.optional "lawEnforcementAgency" lawEnforcementAgency lawEnforcementAgencySchema
      #+ FC.optional "sportOrganization" sportOrganization sportOrganizationSchema
      #+ FC.optional "medicalOrganization" medicalOrganization medicalOrganizationSchema