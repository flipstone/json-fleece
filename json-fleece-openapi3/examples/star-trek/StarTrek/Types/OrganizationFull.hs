{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull
  ( OrganizationFull(..)
  , organizationFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.OrganizationFull.AlternateReality as AlternateReality
import qualified StarTrek.Types.OrganizationFull.Government as Government
import qualified StarTrek.Types.OrganizationFull.GovernmentAgency as GovernmentAgency
import qualified StarTrek.Types.OrganizationFull.IntergovernmentalOrganization as IntergovernmentalOrganization
import qualified StarTrek.Types.OrganizationFull.LawEnforcementAgency as LawEnforcementAgency
import qualified StarTrek.Types.OrganizationFull.MedicalOrganization as MedicalOrganization
import qualified StarTrek.Types.OrganizationFull.MilitaryOrganization as MilitaryOrganization
import qualified StarTrek.Types.OrganizationFull.MilitaryUnit as MilitaryUnit
import qualified StarTrek.Types.OrganizationFull.Mirror as Mirror
import qualified StarTrek.Types.OrganizationFull.Name as Name
import qualified StarTrek.Types.OrganizationFull.PrisonOrPenalColony as PrisonOrPenalColony
import qualified StarTrek.Types.OrganizationFull.ResearchOrganization as ResearchOrganization
import qualified StarTrek.Types.OrganizationFull.SportOrganization as SportOrganization
import qualified StarTrek.Types.OrganizationFull.Uid as Uid

data OrganizationFull = OrganizationFull
  { mirror :: Maybe Mirror.Mirror -- ^ Whether this organization is from mirror universe
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this organization is from alternate reality
  , intergovernmentalOrganization :: Maybe IntergovernmentalOrganization.IntergovernmentalOrganization -- ^ Whether it's an intergovernmental organization
  , militaryUnit :: Maybe MilitaryUnit.MilitaryUnit -- ^ Whether it's a military unit
  , government :: Maybe Government.Government -- ^ Whether it's a government
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , sportOrganization :: Maybe SportOrganization.SportOrganization -- ^ Whether it's a sport organization
  , lawEnforcementAgency :: Maybe LawEnforcementAgency.LawEnforcementAgency -- ^ Whether it's a law enforcement agency
  , prisonOrPenalColony :: Maybe PrisonOrPenalColony.PrisonOrPenalColony -- ^ Whether it's a prison or penal colony
  , uid :: Uid.Uid -- ^ Organization unique ID
  , medicalOrganization :: Maybe MedicalOrganization.MedicalOrganization -- ^ Whether it's a medical organization
  , governmentAgency :: Maybe GovernmentAgency.GovernmentAgency -- ^ Whether it's a government agency
  , name :: Name.Name -- ^ Organization name
  , militaryOrganization :: Maybe MilitaryOrganization.MilitaryOrganization -- ^ Whether it's a military organization
  , researchOrganization :: Maybe ResearchOrganization.ResearchOrganization -- ^ Whether it's a research organization
  }
  deriving (Eq, Show)

organizationFullSchema :: FC.Fleece schema => schema OrganizationFull
organizationFullSchema =
  FC.object $
    FC.constructor OrganizationFull
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "intergovernmentalOrganization" intergovernmentalOrganization IntergovernmentalOrganization.intergovernmentalOrganizationSchema
      #+ FC.optional "militaryUnit" militaryUnit MilitaryUnit.militaryUnitSchema
      #+ FC.optional "government" government Government.governmentSchema
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "sportOrganization" sportOrganization SportOrganization.sportOrganizationSchema
      #+ FC.optional "lawEnforcementAgency" lawEnforcementAgency LawEnforcementAgency.lawEnforcementAgencySchema
      #+ FC.optional "prisonOrPenalColony" prisonOrPenalColony PrisonOrPenalColony.prisonOrPenalColonySchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "medicalOrganization" medicalOrganization MedicalOrganization.medicalOrganizationSchema
      #+ FC.optional "governmentAgency" governmentAgency GovernmentAgency.governmentAgencySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "militaryOrganization" militaryOrganization MilitaryOrganization.militaryOrganizationSchema
      #+ FC.optional "researchOrganization" researchOrganization ResearchOrganization.researchOrganizationSchema