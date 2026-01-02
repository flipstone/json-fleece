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
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this location is from alternate reality
  , bajoranSettlement :: Maybe BajoranSettlement.BajoranSettlement -- ^ Whether it's a Bajoran settlement
  , bodyOfWater :: Maybe BodyOfWater.BodyOfWater -- ^ Whether it's a body of water
  , buildingInterior :: Maybe BuildingInterior.BuildingInterior -- ^ Whether it's a building interior
  , colony :: Maybe Colony.Colony -- ^ Whether it's a colony
  , country :: Maybe Country.Country -- ^ Whether it's a country
  , ds9Establishment :: Maybe Ds9Establishment.Ds9Establishment -- ^ Whether it's a DS9 establishment
  , earthlyLocation :: Maybe EarthlyLocation.EarthlyLocation -- ^ Whether it's an earthly location
  , establishment :: Maybe Establishment.Establishment -- ^ Whether it's a establishment
  , fictionalLocation :: Maybe FictionalLocation.FictionalLocation -- ^ Whether it's a fictional location
  , geographicalLocation :: Maybe GeographicalLocation.GeographicalLocation -- ^ Whether it's a geographical location
  , landform :: Maybe Landform.Landform -- ^ Whether it's a landform
  , landmark :: Maybe Landmark.Landmark -- ^ Whether it's a landmark
  , medicalEstablishment :: Maybe MedicalEstablishment.MedicalEstablishment -- ^ Whether it's a medical establishment
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this location is from mirror universe
  , name :: Name.Name -- ^ Location name
  , religiousLocation :: Maybe ReligiousLocation.ReligiousLocation -- ^ Whether it's a religious location
  , road :: Maybe Road.Road -- ^ Whether it's a road
  , school :: Maybe School.School -- ^ Whether it's a school
  , settlement :: Maybe Settlement.Settlement -- ^ Whether it's a settlement
  , shipyard :: Maybe Shipyard.Shipyard -- ^ Whether it's a shipyard
  , structure :: Maybe Structure.Structure -- ^ Whether it's a structure
  , subnationalEntity :: Maybe SubnationalEntity.SubnationalEntity -- ^ Whether it's a subnational entity
  , uid :: Uid.Uid -- ^ Location unique ID
  , usSettlement :: Maybe UsSettlement.UsSettlement -- ^ Whether it's a US settlement
  }
  deriving (Eq, Show)

locationBaseSchema :: FC.Fleece t => FC.Schema t LocationBase
locationBaseSchema =
  FC.object $
    FC.constructor LocationBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "bajoranSettlement" bajoranSettlement BajoranSettlement.bajoranSettlementSchema
      #+ FC.optional "bodyOfWater" bodyOfWater BodyOfWater.bodyOfWaterSchema
      #+ FC.optional "buildingInterior" buildingInterior BuildingInterior.buildingInteriorSchema
      #+ FC.optional "colony" colony Colony.colonySchema
      #+ FC.optional "country" country Country.countrySchema
      #+ FC.optional "ds9Establishment" ds9Establishment Ds9Establishment.ds9EstablishmentSchema
      #+ FC.optional "earthlyLocation" earthlyLocation EarthlyLocation.earthlyLocationSchema
      #+ FC.optional "establishment" establishment Establishment.establishmentSchema
      #+ FC.optional "fictionalLocation" fictionalLocation FictionalLocation.fictionalLocationSchema
      #+ FC.optional "geographicalLocation" geographicalLocation GeographicalLocation.geographicalLocationSchema
      #+ FC.optional "landform" landform Landform.landformSchema
      #+ FC.optional "landmark" landmark Landmark.landmarkSchema
      #+ FC.optional "medicalEstablishment" medicalEstablishment MedicalEstablishment.medicalEstablishmentSchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "religiousLocation" religiousLocation ReligiousLocation.religiousLocationSchema
      #+ FC.optional "road" road Road.roadSchema
      #+ FC.optional "school" school School.schoolSchema
      #+ FC.optional "settlement" settlement Settlement.settlementSchema
      #+ FC.optional "shipyard" shipyard Shipyard.shipyardSchema
      #+ FC.optional "structure" structure Structure.structureSchema
      #+ FC.optional "subnationalEntity" subnationalEntity SubnationalEntity.subnationalEntitySchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "usSettlement" usSettlement UsSettlement.usSettlementSchema