{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase
  ( TechnologyBase(..)
  , technologyBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TechnologyBase.BorgComponent as BorgComponent
import qualified StarTrek.Types.TechnologyBase.BorgTechnology as BorgTechnology
import qualified StarTrek.Types.TechnologyBase.CommunicationsTechnology as CommunicationsTechnology
import qualified StarTrek.Types.TechnologyBase.ComputerProgramming as ComputerProgramming
import qualified StarTrek.Types.TechnologyBase.ComputerTechnology as ComputerTechnology
import qualified StarTrek.Types.TechnologyBase.CulinaryTool as CulinaryTool
import qualified StarTrek.Types.TechnologyBase.Database as Database
import qualified StarTrek.Types.TechnologyBase.EnergyTechnology as EnergyTechnology
import qualified StarTrek.Types.TechnologyBase.EngineeringTool as EngineeringTool
import qualified StarTrek.Types.TechnologyBase.FictionalTechnology as FictionalTechnology
import qualified StarTrek.Types.TechnologyBase.HolographicTechnology as HolographicTechnology
import qualified StarTrek.Types.TechnologyBase.HouseholdTool as HouseholdTool
import qualified StarTrek.Types.TechnologyBase.IdentificationTechnology as IdentificationTechnology
import qualified StarTrek.Types.TechnologyBase.LifeSupportTechnology as LifeSupportTechnology
import qualified StarTrek.Types.TechnologyBase.MedicalEquipment as MedicalEquipment
import qualified StarTrek.Types.TechnologyBase.Name as Name
import qualified StarTrek.Types.TechnologyBase.SensorTechnology as SensorTechnology
import qualified StarTrek.Types.TechnologyBase.ShieldTechnology as ShieldTechnology
import qualified StarTrek.Types.TechnologyBase.Subroutine as Subroutine
import qualified StarTrek.Types.TechnologyBase.Tool as Tool
import qualified StarTrek.Types.TechnologyBase.TransporterTechnology as TransporterTechnology
import qualified StarTrek.Types.TechnologyBase.Uid as Uid

data TechnologyBase = TechnologyBase
  { name :: Name.Name -- ^ Technology name
  , identificationTechnology :: Maybe IdentificationTechnology.IdentificationTechnology -- ^ Whether it's a identification technology
  , fictionalTechnology :: Maybe FictionalTechnology.FictionalTechnology -- ^ Whether it's a fictional technology
  , subroutine :: Maybe Subroutine.Subroutine -- ^ Whether it's a subroutine
  , communicationsTechnology :: Maybe CommunicationsTechnology.CommunicationsTechnology -- ^ Whether it's a communications technology
  , computerTechnology :: Maybe ComputerTechnology.ComputerTechnology -- ^ Whether it's a computer technology
  , borgComponent :: Maybe BorgComponent.BorgComponent -- ^ Whether it's a Borg component
  , engineeringTool :: Maybe EngineeringTool.EngineeringTool -- ^ Whether it's a engineering tool
  , uid :: Uid.Uid -- ^ Technology unique ID
  , transporterTechnology :: Maybe TransporterTechnology.TransporterTechnology -- ^ Whether it's a transporter technology
  , computerProgramming :: Maybe ComputerProgramming.ComputerProgramming -- ^ Whether it's a technology related to computer programming
  , database :: Maybe Database.Database -- ^ Whether it's a database
  , shieldTechnology :: Maybe ShieldTechnology.ShieldTechnology -- ^ Whether it's a shield technology
  , medicalEquipment :: Maybe MedicalEquipment.MedicalEquipment -- ^ Whether it's a medical equipment
  , holographicTechnology :: Maybe HolographicTechnology.HolographicTechnology -- ^ Whether it's a holographic technology
  , householdTool :: Maybe HouseholdTool.HouseholdTool -- ^ Whether it's a household tool
  , culinaryTool :: Maybe CulinaryTool.CulinaryTool -- ^ Whether it's a culinary tool
  , energyTechnology :: Maybe EnergyTechnology.EnergyTechnology -- ^ Whether it's a energy technology
  , lifeSupportTechnology :: Maybe LifeSupportTechnology.LifeSupportTechnology -- ^ Whether it's a life support technology
  , tool :: Maybe Tool.Tool -- ^ Whether it's a tool
  , borgTechnology :: Maybe BorgTechnology.BorgTechnology -- ^ Whether it's a Borg technology
  , sensorTechnology :: Maybe SensorTechnology.SensorTechnology -- ^ Whether it's a sensor technology
  }
  deriving (Eq, Show)

technologyBaseSchema :: FC.Fleece schema => schema TechnologyBase
technologyBaseSchema =
  FC.object $
    FC.constructor TechnologyBase
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "identificationTechnology" identificationTechnology IdentificationTechnology.identificationTechnologySchema
      #+ FC.optional "fictionalTechnology" fictionalTechnology FictionalTechnology.fictionalTechnologySchema
      #+ FC.optional "subroutine" subroutine Subroutine.subroutineSchema
      #+ FC.optional "communicationsTechnology" communicationsTechnology CommunicationsTechnology.communicationsTechnologySchema
      #+ FC.optional "computerTechnology" computerTechnology ComputerTechnology.computerTechnologySchema
      #+ FC.optional "borgComponent" borgComponent BorgComponent.borgComponentSchema
      #+ FC.optional "engineeringTool" engineeringTool EngineeringTool.engineeringToolSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "transporterTechnology" transporterTechnology TransporterTechnology.transporterTechnologySchema
      #+ FC.optional "computerProgramming" computerProgramming ComputerProgramming.computerProgrammingSchema
      #+ FC.optional "database" database Database.databaseSchema
      #+ FC.optional "shieldTechnology" shieldTechnology ShieldTechnology.shieldTechnologySchema
      #+ FC.optional "medicalEquipment" medicalEquipment MedicalEquipment.medicalEquipmentSchema
      #+ FC.optional "holographicTechnology" holographicTechnology HolographicTechnology.holographicTechnologySchema
      #+ FC.optional "householdTool" householdTool HouseholdTool.householdToolSchema
      #+ FC.optional "culinaryTool" culinaryTool CulinaryTool.culinaryToolSchema
      #+ FC.optional "energyTechnology" energyTechnology EnergyTechnology.energyTechnologySchema
      #+ FC.optional "lifeSupportTechnology" lifeSupportTechnology LifeSupportTechnology.lifeSupportTechnologySchema
      #+ FC.optional "tool" tool Tool.toolSchema
      #+ FC.optional "borgTechnology" borgTechnology BorgTechnology.borgTechnologySchema
      #+ FC.optional "sensorTechnology" sensorTechnology SensorTechnology.sensorTechnologySchema