{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBase
  ( OrganizationBase(..)
  , organizationBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.OrganizationBase.AlternateReality as AlternateReality
import qualified StarTrek.OrganizationBase.Government as Government
import qualified StarTrek.OrganizationBase.GovernmentAgency as GovernmentAgency
import qualified StarTrek.OrganizationBase.IntergovernmentalOrganization as IntergovernmentalOrganization
import qualified StarTrek.OrganizationBase.LawEnforcementAgency as LawEnforcementAgency
import qualified StarTrek.OrganizationBase.MedicalOrganization as MedicalOrganization
import qualified StarTrek.OrganizationBase.MilitaryOrganization as MilitaryOrganization
import qualified StarTrek.OrganizationBase.MilitaryUnit as MilitaryUnit
import qualified StarTrek.OrganizationBase.Mirror as Mirror
import qualified StarTrek.OrganizationBase.Name as Name
import qualified StarTrek.OrganizationBase.PrisonOrPenalColony as PrisonOrPenalColony
import qualified StarTrek.OrganizationBase.ResearchOrganization as ResearchOrganization
import qualified StarTrek.OrganizationBase.SportOrganization as SportOrganization
import qualified StarTrek.OrganizationBase.Uid as Uid

data OrganizationBase = OrganizationBase
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this location is from alternate reality
  , name :: Name.Name -- ^ Organization name
  , militaryOrganization :: Maybe MilitaryOrganization.MilitaryOrganization -- ^ Whether it's a military organization
  , prisonOrPenalColony :: Maybe PrisonOrPenalColony.PrisonOrPenalColony -- ^ Whether it's a prison or penal colony
  , intergovernmentalOrganization :: Maybe IntergovernmentalOrganization.IntergovernmentalOrganization -- ^ Whether it's an intergovernmental organization
  , uid :: Uid.Uid -- ^ Organization unique ID
  , governmentAgency :: Maybe GovernmentAgency.GovernmentAgency -- ^ Whether it's a government agency
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this organization is from mirror universe
  , militaryUnit :: Maybe MilitaryUnit.MilitaryUnit -- ^ Whether it's a military unit
  , researchOrganization :: Maybe ResearchOrganization.ResearchOrganization -- ^ Whether it's a research organization
  , government :: Maybe Government.Government -- ^ Whether it's a government
  , lawEnforcementAgency :: Maybe LawEnforcementAgency.LawEnforcementAgency -- ^ Whether it's a law enforcement agency
  , sportOrganization :: Maybe SportOrganization.SportOrganization -- ^ Whether it's a sport organization
  , medicalOrganization :: Maybe MedicalOrganization.MedicalOrganization -- ^ Whether it's a medical organization
  }
  deriving (Eq, Show)

organizationBaseSchema :: FC.Fleece schema => schema OrganizationBase
organizationBaseSchema =
  FC.object $
    FC.constructor OrganizationBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "militaryOrganization" militaryOrganization MilitaryOrganization.militaryOrganizationSchema
      #+ FC.optional "prisonOrPenalColony" prisonOrPenalColony PrisonOrPenalColony.prisonOrPenalColonySchema
      #+ FC.optional "intergovernmentalOrganization" intergovernmentalOrganization IntergovernmentalOrganization.intergovernmentalOrganizationSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "governmentAgency" governmentAgency GovernmentAgency.governmentAgencySchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "militaryUnit" militaryUnit MilitaryUnit.militaryUnitSchema
      #+ FC.optional "researchOrganization" researchOrganization ResearchOrganization.researchOrganizationSchema
      #+ FC.optional "government" government Government.governmentSchema
      #+ FC.optional "lawEnforcementAgency" lawEnforcementAgency LawEnforcementAgency.lawEnforcementAgencySchema
      #+ FC.optional "sportOrganization" sportOrganization SportOrganization.sportOrganizationSchema
      #+ FC.optional "medicalOrganization" medicalOrganization MedicalOrganization.medicalOrganizationSchema