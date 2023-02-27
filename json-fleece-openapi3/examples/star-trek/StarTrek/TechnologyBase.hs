{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase
  ( TechnologyBase(..)
  , technologyBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TechnologyBase.BorgComponent (BorgComponent, borgComponentSchema)
import StarTrek.TechnologyBase.BorgTechnology (BorgTechnology, borgTechnologySchema)
import StarTrek.TechnologyBase.CommunicationsTechnology (CommunicationsTechnology, communicationsTechnologySchema)
import StarTrek.TechnologyBase.ComputerProgramming (ComputerProgramming, computerProgrammingSchema)
import StarTrek.TechnologyBase.ComputerTechnology (ComputerTechnology, computerTechnologySchema)
import StarTrek.TechnologyBase.CulinaryTool (CulinaryTool, culinaryToolSchema)
import StarTrek.TechnologyBase.Database (Database, databaseSchema)
import StarTrek.TechnologyBase.EnergyTechnology (EnergyTechnology, energyTechnologySchema)
import StarTrek.TechnologyBase.EngineeringTool (EngineeringTool, engineeringToolSchema)
import StarTrek.TechnologyBase.FictionalTechnology (FictionalTechnology, fictionalTechnologySchema)
import StarTrek.TechnologyBase.HolographicTechnology (HolographicTechnology, holographicTechnologySchema)
import StarTrek.TechnologyBase.HouseholdTool (HouseholdTool, householdToolSchema)
import StarTrek.TechnologyBase.IdentificationTechnology (IdentificationTechnology, identificationTechnologySchema)
import StarTrek.TechnologyBase.LifeSupportTechnology (LifeSupportTechnology, lifeSupportTechnologySchema)
import StarTrek.TechnologyBase.MedicalEquipment (MedicalEquipment, medicalEquipmentSchema)
import StarTrek.TechnologyBase.Name (Name, nameSchema)
import StarTrek.TechnologyBase.SensorTechnology (SensorTechnology, sensorTechnologySchema)
import StarTrek.TechnologyBase.ShieldTechnology (ShieldTechnology, shieldTechnologySchema)
import StarTrek.TechnologyBase.Subroutine (Subroutine, subroutineSchema)
import StarTrek.TechnologyBase.Tool (Tool, toolSchema)
import StarTrek.TechnologyBase.TransporterTechnology (TransporterTechnology, transporterTechnologySchema)
import StarTrek.TechnologyBase.Uid (Uid, uidSchema)

data TechnologyBase = TechnologyBase
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

technologyBaseSchema :: FC.Fleece schema => schema TechnologyBase
technologyBaseSchema =
  FC.object $
    FC.constructor TechnologyBase
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