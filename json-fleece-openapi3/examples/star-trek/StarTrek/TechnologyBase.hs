{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase
  ( TechnologyBase(..)
  , technologyBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)

data TechnologyBase = TechnologyBase
  { name :: Text -- ^ Technology name
  , identificationTechnology :: Maybe Bool -- ^ Whether it's a identification technology
  , fictionalTechnology :: Maybe Bool -- ^ Whether it's a fictional technology
  , subroutine :: Maybe Bool -- ^ Whether it's a subroutine
  , communicationsTechnology :: Maybe Bool -- ^ Whether it's a communications technology
  , computerTechnology :: Maybe Bool -- ^ Whether it's a computer technology
  , borgComponent :: Maybe Bool -- ^ Whether it's a Borg component
  , engineeringTool :: Maybe Bool -- ^ Whether it's a engineering tool
  , uid :: Text -- ^ Technology unique ID
  , transporterTechnology :: Maybe Bool -- ^ Whether it's a transporter technology
  , computerProgramming :: Maybe Bool -- ^ Whether it's a technology related to computer programming
  , database :: Maybe Bool -- ^ Whether it's a database
  , shieldTechnology :: Maybe Bool -- ^ Whether it's a shield technology
  , medicalEquipment :: Maybe Bool -- ^ Whether it's a medical equipment
  , holographicTechnology :: Maybe Bool -- ^ Whether it's a holographic technology
  , householdTool :: Maybe Bool -- ^ Whether it's a household tool
  , culinaryTool :: Maybe Bool -- ^ Whether it's a culinary tool
  , energyTechnology :: Maybe Bool -- ^ Whether it's a energy technology
  , lifeSupportTechnology :: Maybe Bool -- ^ Whether it's a life support technology
  , tool :: Maybe Bool -- ^ Whether it's a tool
  , borgTechnology :: Maybe Bool -- ^ Whether it's a Borg technology
  , sensorTechnology :: Maybe Bool -- ^ Whether it's a sensor technology
  }
  deriving (Eq, Show)

technologyBaseSchema :: FC.Fleece schema => schema TechnologyBase
technologyBaseSchema =
  FC.object $
    FC.constructor TechnologyBase
      #+ FC.required "name" name FC.text
      #+ FC.optional "identificationTechnology" identificationTechnology FC.boolean
      #+ FC.optional "fictionalTechnology" fictionalTechnology FC.boolean
      #+ FC.optional "subroutine" subroutine FC.boolean
      #+ FC.optional "communicationsTechnology" communicationsTechnology FC.boolean
      #+ FC.optional "computerTechnology" computerTechnology FC.boolean
      #+ FC.optional "borgComponent" borgComponent FC.boolean
      #+ FC.optional "engineeringTool" engineeringTool FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "transporterTechnology" transporterTechnology FC.boolean
      #+ FC.optional "computerProgramming" computerProgramming FC.boolean
      #+ FC.optional "database" database FC.boolean
      #+ FC.optional "shieldTechnology" shieldTechnology FC.boolean
      #+ FC.optional "medicalEquipment" medicalEquipment FC.boolean
      #+ FC.optional "holographicTechnology" holographicTechnology FC.boolean
      #+ FC.optional "householdTool" householdTool FC.boolean
      #+ FC.optional "culinaryTool" culinaryTool FC.boolean
      #+ FC.optional "energyTechnology" energyTechnology FC.boolean
      #+ FC.optional "lifeSupportTechnology" lifeSupportTechnology FC.boolean
      #+ FC.optional "tool" tool FC.boolean
      #+ FC.optional "borgTechnology" borgTechnology FC.boolean
      #+ FC.optional "sensorTechnology" sensorTechnology FC.boolean