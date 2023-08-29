{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase
  ( LocationBase(..)
  , locationBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.LocationBase.AlternateReality as AlternateReality
import qualified StarTrek.Types.LocationBase.BajoranSettlement as BajoranSettlement
import qualified StarTrek.Types.LocationBase.BodyOfWater as BodyOfWater
import qualified StarTrek.Types.LocationBase.BuildingInterior as BuildingInterior
import qualified StarTrek.Types.LocationBase.Colony as Colony
import qualified StarTrek.Types.LocationBase.Country as Country
import qualified StarTrek.Types.LocationBase.Ds9Establishment as Ds9Establishment
import qualified StarTrek.Types.LocationBase.EarthlyLocation as EarthlyLocation
import qualified StarTrek.Types.LocationBase.Establishment as Establishment
import qualified StarTrek.Types.LocationBase.FictionalLocation as FictionalLocation
import qualified StarTrek.Types.LocationBase.GeographicalLocation as GeographicalLocation
import qualified StarTrek.Types.LocationBase.Landform as Landform
import qualified StarTrek.Types.LocationBase.Landmark as Landmark
import qualified StarTrek.Types.LocationBase.MedicalEstablishment as MedicalEstablishment
import qualified StarTrek.Types.LocationBase.Mirror as Mirror
import qualified StarTrek.Types.LocationBase.Name as Name
import qualified StarTrek.Types.LocationBase.ReligiousLocation as ReligiousLocation
import qualified StarTrek.Types.LocationBase.Road as Road
import qualified StarTrek.Types.LocationBase.School as School
import qualified StarTrek.Types.LocationBase.Settlement as Settlement
import qualified StarTrek.Types.LocationBase.Shipyard as Shipyard
import qualified StarTrek.Types.LocationBase.Structure as Structure
import qualified StarTrek.Types.LocationBase.SubnationalEntity as SubnationalEntity
import qualified StarTrek.Types.LocationBase.Uid as Uid
import qualified StarTrek.Types.LocationBase.UsSettlement as UsSettlement

data LocationBase = LocationBase
  { usSettlement :: Maybe UsSettlement.UsSettlement -- ^ Whether it's a US settlement
  , buildingInterior :: Maybe BuildingInterior.BuildingInterior -- ^ Whether it's a building interior
  , subnationalEntity :: Maybe SubnationalEntity.SubnationalEntity -- ^ Whether it's a subnational entity
  , landmark :: Maybe Landmark.Landmark -- ^ Whether it's a landmark
  , geographicalLocation :: Maybe GeographicalLocation.GeographicalLocation -- ^ Whether it's a geographical location
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this location is from mirror universe
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this location is from alternate reality
  , structure :: Maybe Structure.Structure -- ^ Whether it's a structure
  , landform :: Maybe Landform.Landform -- ^ Whether it's a landform
  , religiousLocation :: Maybe ReligiousLocation.ReligiousLocation -- ^ Whether it's a religious location
  , school :: Maybe School.School -- ^ Whether it's a school
  , ds9Establishment :: Maybe Ds9Establishment.Ds9Establishment -- ^ Whether it's a DS9 establishment
  , establishment :: Maybe Establishment.Establishment -- ^ Whether it's a establishment
  , shipyard :: Maybe Shipyard.Shipyard -- ^ Whether it's a shipyard
  , country :: Maybe Country.Country -- ^ Whether it's a country
  , fictionalLocation :: Maybe FictionalLocation.FictionalLocation -- ^ Whether it's a fictional location
  , uid :: Uid.Uid -- ^ Location unique ID
  , colony :: Maybe Colony.Colony -- ^ Whether it's a colony
  , bajoranSettlement :: Maybe BajoranSettlement.BajoranSettlement -- ^ Whether it's a Bajoran settlement
  , settlement :: Maybe Settlement.Settlement -- ^ Whether it's a settlement
  , name :: Name.Name -- ^ Location name
  , bodyOfWater :: Maybe BodyOfWater.BodyOfWater -- ^ Whether it's a body of water
  , road :: Maybe Road.Road -- ^ Whether it's a road
  , medicalEstablishment :: Maybe MedicalEstablishment.MedicalEstablishment -- ^ Whether it's a medical establishment
  , earthlyLocation :: Maybe EarthlyLocation.EarthlyLocation -- ^ Whether it's an earthly location
  }
  deriving (Eq, Show)

locationBaseSchema :: FC.Fleece schema => schema LocationBase
locationBaseSchema =
  FC.object $
    FC.constructor LocationBase
      #+ FC.optional "usSettlement" usSettlement UsSettlement.usSettlementSchema
      #+ FC.optional "buildingInterior" buildingInterior BuildingInterior.buildingInteriorSchema
      #+ FC.optional "subnationalEntity" subnationalEntity SubnationalEntity.subnationalEntitySchema
      #+ FC.optional "landmark" landmark Landmark.landmarkSchema
      #+ FC.optional "geographicalLocation" geographicalLocation GeographicalLocation.geographicalLocationSchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "structure" structure Structure.structureSchema
      #+ FC.optional "landform" landform Landform.landformSchema
      #+ FC.optional "religiousLocation" religiousLocation ReligiousLocation.religiousLocationSchema
      #+ FC.optional "school" school School.schoolSchema
      #+ FC.optional "ds9Establishment" ds9Establishment Ds9Establishment.ds9EstablishmentSchema
      #+ FC.optional "establishment" establishment Establishment.establishmentSchema
      #+ FC.optional "shipyard" shipyard Shipyard.shipyardSchema
      #+ FC.optional "country" country Country.countrySchema
      #+ FC.optional "fictionalLocation" fictionalLocation FictionalLocation.fictionalLocationSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "colony" colony Colony.colonySchema
      #+ FC.optional "bajoranSettlement" bajoranSettlement BajoranSettlement.bajoranSettlementSchema
      #+ FC.optional "settlement" settlement Settlement.settlementSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "bodyOfWater" bodyOfWater BodyOfWater.bodyOfWaterSchema
      #+ FC.optional "road" road Road.roadSchema
      #+ FC.optional "medicalEstablishment" medicalEstablishment MedicalEstablishment.medicalEstablishmentSchema
      #+ FC.optional "earthlyLocation" earthlyLocation EarthlyLocation.earthlyLocationSchema