{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull
  ( TechnologyFull(..)
  , technologyFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TechnologyFull.BorgComponent as BorgComponent
import qualified StarTrek.Types.TechnologyFull.BorgTechnology as BorgTechnology
import qualified StarTrek.Types.TechnologyFull.CommunicationsTechnology as CommunicationsTechnology
import qualified StarTrek.Types.TechnologyFull.ComputerProgramming as ComputerProgramming
import qualified StarTrek.Types.TechnologyFull.ComputerTechnology as ComputerTechnology
import qualified StarTrek.Types.TechnologyFull.CulinaryTool as CulinaryTool
import qualified StarTrek.Types.TechnologyFull.Database as Database
import qualified StarTrek.Types.TechnologyFull.EnergyTechnology as EnergyTechnology
import qualified StarTrek.Types.TechnologyFull.EngineeringTool as EngineeringTool
import qualified StarTrek.Types.TechnologyFull.FictionalTechnology as FictionalTechnology
import qualified StarTrek.Types.TechnologyFull.HolographicTechnology as HolographicTechnology
import qualified StarTrek.Types.TechnologyFull.HouseholdTool as HouseholdTool
import qualified StarTrek.Types.TechnologyFull.IdentificationTechnology as IdentificationTechnology
import qualified StarTrek.Types.TechnologyFull.LifeSupportTechnology as LifeSupportTechnology
import qualified StarTrek.Types.TechnologyFull.MedicalEquipment as MedicalEquipment
import qualified StarTrek.Types.TechnologyFull.Name as Name
import qualified StarTrek.Types.TechnologyFull.SensorTechnology as SensorTechnology
import qualified StarTrek.Types.TechnologyFull.ShieldTechnology as ShieldTechnology
import qualified StarTrek.Types.TechnologyFull.Subroutine as Subroutine
import qualified StarTrek.Types.TechnologyFull.Tool as Tool
import qualified StarTrek.Types.TechnologyFull.TransporterTechnology as TransporterTechnology
import qualified StarTrek.Types.TechnologyFull.Uid as Uid

data TechnologyFull = TechnologyFull
  { borgComponent :: Maybe BorgComponent.BorgComponent -- ^ Whether it's a Borg component
  , borgTechnology :: Maybe BorgTechnology.BorgTechnology -- ^ Whether it's a Borg technology
  , communicationsTechnology :: Maybe CommunicationsTechnology.CommunicationsTechnology -- ^ Whether it's a communications technology
  , computerProgramming :: Maybe ComputerProgramming.ComputerProgramming -- ^ Whether it's a technology related to computer programming
  , computerTechnology :: Maybe ComputerTechnology.ComputerTechnology -- ^ Whether it's a computer technology
  , culinaryTool :: Maybe CulinaryTool.CulinaryTool -- ^ Whether it's a culinary tool
  , database :: Maybe Database.Database -- ^ Whether it's a database
  , energyTechnology :: Maybe EnergyTechnology.EnergyTechnology -- ^ Whether it's a energy technology
  , engineeringTool :: Maybe EngineeringTool.EngineeringTool -- ^ Whether it's a engineering tool
  , fictionalTechnology :: Maybe FictionalTechnology.FictionalTechnology -- ^ Whether it's a fictional technology
  , holographicTechnology :: Maybe HolographicTechnology.HolographicTechnology -- ^ Whether it's a holographic technology
  , householdTool :: Maybe HouseholdTool.HouseholdTool -- ^ Whether it's a household tool
  , identificationTechnology :: Maybe IdentificationTechnology.IdentificationTechnology -- ^ Whether it's a identification technology
  , lifeSupportTechnology :: Maybe LifeSupportTechnology.LifeSupportTechnology -- ^ Whether it's a life support technology
  , medicalEquipment :: Maybe MedicalEquipment.MedicalEquipment -- ^ Whether it's a medical equipment
  , name :: Name.Name -- ^ Technology name
  , sensorTechnology :: Maybe SensorTechnology.SensorTechnology -- ^ Whether it's a sensor technology
  , shieldTechnology :: Maybe ShieldTechnology.ShieldTechnology -- ^ Whether it's a shield technology
  , subroutine :: Maybe Subroutine.Subroutine -- ^ Whether it's a subroutine
  , tool :: Maybe Tool.Tool -- ^ Whether it's a tool
  , transporterTechnology :: Maybe TransporterTechnology.TransporterTechnology -- ^ Whether it's a transporter technology
  , uid :: Uid.Uid -- ^ Technology unique ID
  }
  deriving (Eq, Show)

technologyFullSchema :: FC.Fleece schema => schema TechnologyFull
technologyFullSchema =
  FC.object $
    FC.constructor TechnologyFull
      #+ FC.optional "borgComponent" borgComponent BorgComponent.borgComponentSchema
      #+ FC.optional "borgTechnology" borgTechnology BorgTechnology.borgTechnologySchema
      #+ FC.optional "communicationsTechnology" communicationsTechnology CommunicationsTechnology.communicationsTechnologySchema
      #+ FC.optional "computerProgramming" computerProgramming ComputerProgramming.computerProgrammingSchema
      #+ FC.optional "computerTechnology" computerTechnology ComputerTechnology.computerTechnologySchema
      #+ FC.optional "culinaryTool" culinaryTool CulinaryTool.culinaryToolSchema
      #+ FC.optional "database" database Database.databaseSchema
      #+ FC.optional "energyTechnology" energyTechnology EnergyTechnology.energyTechnologySchema
      #+ FC.optional "engineeringTool" engineeringTool EngineeringTool.engineeringToolSchema
      #+ FC.optional "fictionalTechnology" fictionalTechnology FictionalTechnology.fictionalTechnologySchema
      #+ FC.optional "holographicTechnology" holographicTechnology HolographicTechnology.holographicTechnologySchema
      #+ FC.optional "householdTool" householdTool HouseholdTool.householdToolSchema
      #+ FC.optional "identificationTechnology" identificationTechnology IdentificationTechnology.identificationTechnologySchema
      #+ FC.optional "lifeSupportTechnology" lifeSupportTechnology LifeSupportTechnology.lifeSupportTechnologySchema
      #+ FC.optional "medicalEquipment" medicalEquipment MedicalEquipment.medicalEquipmentSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "sensorTechnology" sensorTechnology SensorTechnology.sensorTechnologySchema
      #+ FC.optional "shieldTechnology" shieldTechnology ShieldTechnology.shieldTechnologySchema
      #+ FC.optional "subroutine" subroutine Subroutine.subroutineSchema
      #+ FC.optional "tool" tool Tool.toolSchema
      #+ FC.optional "transporterTechnology" transporterTechnology TransporterTechnology.transporterTechnologySchema
      #+ FC.required "uid" uid Uid.uidSchema