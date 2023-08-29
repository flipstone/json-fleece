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
  { sensorTechnology :: Maybe SensorTechnology.SensorTechnology -- ^ Whether it's a sensor technology
  , borgTechnology :: Maybe BorgTechnology.BorgTechnology -- ^ Whether it's a Borg technology
  , culinaryTool :: Maybe CulinaryTool.CulinaryTool -- ^ Whether it's a culinary tool
  , database :: Maybe Database.Database -- ^ Whether it's a database
  , subroutine :: Maybe Subroutine.Subroutine -- ^ Whether it's a subroutine
  , identificationTechnology :: Maybe IdentificationTechnology.IdentificationTechnology -- ^ Whether it's a identification technology
  , borgComponent :: Maybe BorgComponent.BorgComponent -- ^ Whether it's a Borg component
  , medicalEquipment :: Maybe MedicalEquipment.MedicalEquipment -- ^ Whether it's a medical equipment
  , energyTechnology :: Maybe EnergyTechnology.EnergyTechnology -- ^ Whether it's a energy technology
  , holographicTechnology :: Maybe HolographicTechnology.HolographicTechnology -- ^ Whether it's a holographic technology
  , uid :: Uid.Uid -- ^ Technology unique ID
  , householdTool :: Maybe HouseholdTool.HouseholdTool -- ^ Whether it's a household tool
  , lifeSupportTechnology :: Maybe LifeSupportTechnology.LifeSupportTechnology -- ^ Whether it's a life support technology
  , transporterTechnology :: Maybe TransporterTechnology.TransporterTechnology -- ^ Whether it's a transporter technology
  , engineeringTool :: Maybe EngineeringTool.EngineeringTool -- ^ Whether it's a engineering tool
  , name :: Name.Name -- ^ Technology name
  , fictionalTechnology :: Maybe FictionalTechnology.FictionalTechnology -- ^ Whether it's a fictional technology
  , communicationsTechnology :: Maybe CommunicationsTechnology.CommunicationsTechnology -- ^ Whether it's a communications technology
  , computerProgramming :: Maybe ComputerProgramming.ComputerProgramming -- ^ Whether it's a technology related to computer programming
  , shieldTechnology :: Maybe ShieldTechnology.ShieldTechnology -- ^ Whether it's a shield technology
  , computerTechnology :: Maybe ComputerTechnology.ComputerTechnology -- ^ Whether it's a computer technology
  , tool :: Maybe Tool.Tool -- ^ Whether it's a tool
  }
  deriving (Eq, Show)

technologyFullSchema :: FC.Fleece schema => schema TechnologyFull
technologyFullSchema =
  FC.object $
    FC.constructor TechnologyFull
      #+ FC.optional "sensorTechnology" sensorTechnology SensorTechnology.sensorTechnologySchema
      #+ FC.optional "borgTechnology" borgTechnology BorgTechnology.borgTechnologySchema
      #+ FC.optional "culinaryTool" culinaryTool CulinaryTool.culinaryToolSchema
      #+ FC.optional "database" database Database.databaseSchema
      #+ FC.optional "subroutine" subroutine Subroutine.subroutineSchema
      #+ FC.optional "identificationTechnology" identificationTechnology IdentificationTechnology.identificationTechnologySchema
      #+ FC.optional "borgComponent" borgComponent BorgComponent.borgComponentSchema
      #+ FC.optional "medicalEquipment" medicalEquipment MedicalEquipment.medicalEquipmentSchema
      #+ FC.optional "energyTechnology" energyTechnology EnergyTechnology.energyTechnologySchema
      #+ FC.optional "holographicTechnology" holographicTechnology HolographicTechnology.holographicTechnologySchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "householdTool" householdTool HouseholdTool.householdToolSchema
      #+ FC.optional "lifeSupportTechnology" lifeSupportTechnology LifeSupportTechnology.lifeSupportTechnologySchema
      #+ FC.optional "transporterTechnology" transporterTechnology TransporterTechnology.transporterTechnologySchema
      #+ FC.optional "engineeringTool" engineeringTool EngineeringTool.engineeringToolSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "fictionalTechnology" fictionalTechnology FictionalTechnology.fictionalTechnologySchema
      #+ FC.optional "communicationsTechnology" communicationsTechnology CommunicationsTechnology.communicationsTechnologySchema
      #+ FC.optional "computerProgramming" computerProgramming ComputerProgramming.computerProgrammingSchema
      #+ FC.optional "shieldTechnology" shieldTechnology ShieldTechnology.shieldTechnologySchema
      #+ FC.optional "computerTechnology" computerTechnology ComputerTechnology.computerTechnologySchema
      #+ FC.optional "tool" tool Tool.toolSchema