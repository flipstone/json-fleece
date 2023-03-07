{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull
  ( LocationFull(..)
  , locationFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.LocationFull.AlternateReality as AlternateReality
import qualified StarTrek.Types.LocationFull.BajoranSettlement as BajoranSettlement
import qualified StarTrek.Types.LocationFull.BodyOfWater as BodyOfWater
import qualified StarTrek.Types.LocationFull.BuildingInterior as BuildingInterior
import qualified StarTrek.Types.LocationFull.Colony as Colony
import qualified StarTrek.Types.LocationFull.Country as Country
import qualified StarTrek.Types.LocationFull.Ds9Establishment as Ds9Establishment
import qualified StarTrek.Types.LocationFull.EarthlyLocation as EarthlyLocation
import qualified StarTrek.Types.LocationFull.Establishment as Establishment
import qualified StarTrek.Types.LocationFull.FictionalLocation as FictionalLocation
import qualified StarTrek.Types.LocationFull.GeographicalLocation as GeographicalLocation
import qualified StarTrek.Types.LocationFull.Landform as Landform
import qualified StarTrek.Types.LocationFull.Landmark as Landmark
import qualified StarTrek.Types.LocationFull.MedicalEstablishment as MedicalEstablishment
import qualified StarTrek.Types.LocationFull.Mirror as Mirror
import qualified StarTrek.Types.LocationFull.Name as Name
import qualified StarTrek.Types.LocationFull.ReligiousLocation as ReligiousLocation
import qualified StarTrek.Types.LocationFull.Road as Road
import qualified StarTrek.Types.LocationFull.School as School
import qualified StarTrek.Types.LocationFull.Settlement as Settlement
import qualified StarTrek.Types.LocationFull.Shipyard as Shipyard
import qualified StarTrek.Types.LocationFull.Structure as Structure
import qualified StarTrek.Types.LocationFull.SubnationalEntity as SubnationalEntity
import qualified StarTrek.Types.LocationFull.Uid as Uid
import qualified StarTrek.Types.LocationFull.UsSettlement as UsSettlement

data LocationFull = LocationFull
  { bodyOfWater :: Maybe BodyOfWater.BodyOfWater -- ^ Whether it's a body of water
  , establishment :: Maybe Establishment.Establishment -- ^ Whether it's a establishment
  , alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this location is from alternate reality
  , name :: Name.Name -- ^ Location name
  , fictionalLocation :: Maybe FictionalLocation.FictionalLocation -- ^ Whether it's a fictional location
  , usSettlement :: Maybe UsSettlement.UsSettlement -- ^ Whether it's a US settlement
  , country :: Maybe Country.Country -- ^ Whether it's a country
  , colony :: Maybe Colony.Colony -- ^ Whether it's a colony
  , ds9Establishment :: Maybe Ds9Establishment.Ds9Establishment -- ^ Whether it's a DS9 establishment
  , road :: Maybe Road.Road -- ^ Whether it's a road
  , uid :: Uid.Uid -- ^ Location unique ID
  , medicalEstablishment :: Maybe MedicalEstablishment.MedicalEstablishment -- ^ Whether it's a medical establishment
  , earthlyLocation :: Maybe EarthlyLocation.EarthlyLocation -- ^ Whether it's an earthly location
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this location is from mirror universe
  , settlement :: Maybe Settlement.Settlement -- ^ Whether it's a settlement
  , buildingInterior :: Maybe BuildingInterior.BuildingInterior -- ^ Whether it's a building interior
  , subnationalEntity :: Maybe SubnationalEntity.SubnationalEntity -- ^ Whether it's a subnational entity
  , landform :: Maybe Landform.Landform -- ^ Whether it's a landform
  , school :: Maybe School.School -- ^ Whether it's a school
  , landmark :: Maybe Landmark.Landmark -- ^ Whether it's a landmark
  , religiousLocation :: Maybe ReligiousLocation.ReligiousLocation -- ^ Whether it's a religious location
  , geographicalLocation :: Maybe GeographicalLocation.GeographicalLocation -- ^ Whether it's a geographical location
  , shipyard :: Maybe Shipyard.Shipyard -- ^ Whether it's a shipyard
  , bajoranSettlement :: Maybe BajoranSettlement.BajoranSettlement -- ^ Whether it's a Bajoran settlement
  , structure :: Maybe Structure.Structure -- ^ Whether it's a structure
  }
  deriving (Eq, Show)

locationFullSchema :: FC.Fleece schema => schema LocationFull
locationFullSchema =
  FC.object $
    FC.constructor LocationFull
      #+ FC.optional "bodyOfWater" bodyOfWater BodyOfWater.bodyOfWaterSchema
      #+ FC.optional "establishment" establishment Establishment.establishmentSchema
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "fictionalLocation" fictionalLocation FictionalLocation.fictionalLocationSchema
      #+ FC.optional "usSettlement" usSettlement UsSettlement.usSettlementSchema
      #+ FC.optional "country" country Country.countrySchema
      #+ FC.optional "colony" colony Colony.colonySchema
      #+ FC.optional "ds9Establishment" ds9Establishment Ds9Establishment.ds9EstablishmentSchema
      #+ FC.optional "road" road Road.roadSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "medicalEstablishment" medicalEstablishment MedicalEstablishment.medicalEstablishmentSchema
      #+ FC.optional "earthlyLocation" earthlyLocation EarthlyLocation.earthlyLocationSchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "settlement" settlement Settlement.settlementSchema
      #+ FC.optional "buildingInterior" buildingInterior BuildingInterior.buildingInteriorSchema
      #+ FC.optional "subnationalEntity" subnationalEntity SubnationalEntity.subnationalEntitySchema
      #+ FC.optional "landform" landform Landform.landformSchema
      #+ FC.optional "school" school School.schoolSchema
      #+ FC.optional "landmark" landmark Landmark.landmarkSchema
      #+ FC.optional "religiousLocation" religiousLocation ReligiousLocation.religiousLocationSchema
      #+ FC.optional "geographicalLocation" geographicalLocation GeographicalLocation.geographicalLocationSchema
      #+ FC.optional "shipyard" shipyard Shipyard.shipyardSchema
      #+ FC.optional "bajoranSettlement" bajoranSettlement BajoranSettlement.bajoranSettlementSchema
      #+ FC.optional "structure" structure Structure.structureSchema