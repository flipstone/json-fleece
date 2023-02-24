{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase
  ( TechnologyBase(..)
  , technologyBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "identificationTechnology" identificationTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "fictionalTechnology" fictionalTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "subroutine" subroutine FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "communicationsTechnology" communicationsTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "computerTechnology" computerTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "borgComponent" borgComponent FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "engineeringTool" engineeringTool FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "transporterTechnology" transporterTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "computerProgramming" computerProgramming FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "database" database FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "shieldTechnology" shieldTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "medicalEquipment" medicalEquipment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "holographicTechnology" holographicTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "householdTool" householdTool FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "culinaryTool" culinaryTool FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "energyTechnology" energyTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "lifeSupportTechnology" lifeSupportTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "tool" tool FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "borgTechnology" borgTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "sensorTechnology" sensorTechnology FC.boolean