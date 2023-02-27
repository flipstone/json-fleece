{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull
  ( TechnologyFull(..)
  , technologyFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TechnologyFull.BorgComponent (BorgComponent, borgComponentSchema)
import StarTrek.TechnologyFull.BorgTechnology (BorgTechnology, borgTechnologySchema)
import StarTrek.TechnologyFull.CommunicationsTechnology (CommunicationsTechnology, communicationsTechnologySchema)
import StarTrek.TechnologyFull.ComputerProgramming (ComputerProgramming, computerProgrammingSchema)
import StarTrek.TechnologyFull.ComputerTechnology (ComputerTechnology, computerTechnologySchema)
import StarTrek.TechnologyFull.CulinaryTool (CulinaryTool, culinaryToolSchema)
import StarTrek.TechnologyFull.Database (Database, databaseSchema)
import StarTrek.TechnologyFull.EnergyTechnology (EnergyTechnology, energyTechnologySchema)
import StarTrek.TechnologyFull.EngineeringTool (EngineeringTool, engineeringToolSchema)
import StarTrek.TechnologyFull.FictionalTechnology (FictionalTechnology, fictionalTechnologySchema)
import StarTrek.TechnologyFull.HolographicTechnology (HolographicTechnology, holographicTechnologySchema)
import StarTrek.TechnologyFull.HouseholdTool (HouseholdTool, householdToolSchema)
import StarTrek.TechnologyFull.IdentificationTechnology (IdentificationTechnology, identificationTechnologySchema)
import StarTrek.TechnologyFull.LifeSupportTechnology (LifeSupportTechnology, lifeSupportTechnologySchema)
import StarTrek.TechnologyFull.MedicalEquipment (MedicalEquipment, medicalEquipmentSchema)
import StarTrek.TechnologyFull.Name (Name, nameSchema)
import StarTrek.TechnologyFull.SensorTechnology (SensorTechnology, sensorTechnologySchema)
import StarTrek.TechnologyFull.ShieldTechnology (ShieldTechnology, shieldTechnologySchema)
import StarTrek.TechnologyFull.Subroutine (Subroutine, subroutineSchema)
import StarTrek.TechnologyFull.Tool (Tool, toolSchema)
import StarTrek.TechnologyFull.TransporterTechnology (TransporterTechnology, transporterTechnologySchema)
import StarTrek.TechnologyFull.Uid (Uid, uidSchema)

data TechnologyFull = TechnologyFull
  { name :: Name -- ^ Technology name
  , identificationTechnology :: Maybe IdentificationTechnology -- ^ Whether it's a identification technology
  , fictionalTechnology :: Maybe FictionalTechnology -- ^ Whether it's a fictional technology
  , subroutine :: Maybe Subroutine -- ^ Whether it's a subroutine
  , communicationsTechnology :: Maybe CommunicationsTechnology -- ^ Whether it's a communications technology
  , computerTechnology :: Maybe ComputerTechnology -- ^ Whether it's a computer technology
  , borgComponent :: Maybe BorgComponent -- ^ Whether it's a Borg component
  , engineeringTool :: Maybe EngineeringTool -- ^ Whether it's a engineering tool
  , uid :: Uid -- ^ Technology unique ID
  , transporterTechnology :: Maybe TransporterTechnology -- ^ Whether it's a transporter technology
  , computerProgramming :: Maybe ComputerProgramming -- ^ Whether it's a technology related to computer programming
  , database :: Maybe Database -- ^ Whether it's a database
  , shieldTechnology :: Maybe ShieldTechnology -- ^ Whether it's a shield technology
  , medicalEquipment :: Maybe MedicalEquipment -- ^ Whether it's a medical equipment
  , holographicTechnology :: Maybe HolographicTechnology -- ^ Whether it's a holographic technology
  , householdTool :: Maybe HouseholdTool -- ^ Whether it's a household tool
  , culinaryTool :: Maybe CulinaryTool -- ^ Whether it's a culinary tool
  , energyTechnology :: Maybe EnergyTechnology -- ^ Whether it's a energy technology
  , lifeSupportTechnology :: Maybe LifeSupportTechnology -- ^ Whether it's a life support technology
  , tool :: Maybe Tool -- ^ Whether it's a tool
  , borgTechnology :: Maybe BorgTechnology -- ^ Whether it's a Borg technology
  , sensorTechnology :: Maybe SensorTechnology -- ^ Whether it's a sensor technology
  }
  deriving (Eq, Show)

technologyFullSchema :: FC.Fleece schema => schema TechnologyFull
technologyFullSchema =
  FC.object $
    FC.constructor TechnologyFull
      #+ FC.required "name" name nameSchema
      #+ FC.optional "identificationTechnology" identificationTechnology identificationTechnologySchema
      #+ FC.optional "fictionalTechnology" fictionalTechnology fictionalTechnologySchema
      #+ FC.optional "subroutine" subroutine subroutineSchema
      #+ FC.optional "communicationsTechnology" communicationsTechnology communicationsTechnologySchema
      #+ FC.optional "computerTechnology" computerTechnology computerTechnologySchema
      #+ FC.optional "borgComponent" borgComponent borgComponentSchema
      #+ FC.optional "engineeringTool" engineeringTool engineeringToolSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "transporterTechnology" transporterTechnology transporterTechnologySchema
      #+ FC.optional "computerProgramming" computerProgramming computerProgrammingSchema
      #+ FC.optional "database" database databaseSchema
      #+ FC.optional "shieldTechnology" shieldTechnology shieldTechnologySchema
      #+ FC.optional "medicalEquipment" medicalEquipment medicalEquipmentSchema
      #+ FC.optional "holographicTechnology" holographicTechnology holographicTechnologySchema
      #+ FC.optional "householdTool" householdTool householdToolSchema
      #+ FC.optional "culinaryTool" culinaryTool culinaryToolSchema
      #+ FC.optional "energyTechnology" energyTechnology energyTechnologySchema
      #+ FC.optional "lifeSupportTechnology" lifeSupportTechnology lifeSupportTechnologySchema
      #+ FC.optional "tool" tool toolSchema
      #+ FC.optional "borgTechnology" borgTechnology borgTechnologySchema
      #+ FC.optional "sensorTechnology" sensorTechnology sensorTechnologySchema