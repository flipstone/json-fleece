{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBase
  ( LocationBase(..)
  , locationBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data LocationBase = LocationBase
  { bodyOfWater :: Maybe Bool -- ^ Whether it's a body of water
  , establishment :: Maybe Bool -- ^ Whether it's a establishment
  , alternateReality :: Maybe Bool -- ^ Whether this location is from alternate reality
  , name :: Text -- ^ Location name
  , fictionalLocation :: Maybe Bool -- ^ Whether it's a fictional location
  , usSettlement :: Maybe Bool -- ^ Whether it's a US settlement
  , country :: Maybe Bool -- ^ Whether it's a country
  , colony :: Maybe Bool -- ^ Whether it's a colony
  , ds9Establishment :: Maybe Bool -- ^ Whether it's a DS9 establishment
  , road :: Maybe Bool -- ^ Whether it's a road
  , uid :: Text -- ^ Location unique ID
  , medicalEstablishment :: Maybe Bool -- ^ Whether it's a medical establishment
  , earthlyLocation :: Maybe Bool -- ^ Whether it's an earthly location
  , mirror :: Maybe Bool -- ^ Whether this location is from mirror universe
  , settlement :: Maybe Bool -- ^ Whether it's a settlement
  , buildingInterior :: Maybe Bool -- ^ Whether it's a building interior
  , subnationalEntity :: Maybe Bool -- ^ Whether it's a subnational entity
  , landform :: Maybe Bool -- ^ Whether it's a landform
  , school :: Maybe Bool -- ^ Whether it's a school
  , landmark :: Maybe Bool -- ^ Whether it's a landmark
  , religiousLocation :: Maybe Bool -- ^ Whether it's a religious location
  , geographicalLocation :: Maybe Bool -- ^ Whether it's a geographical location
  , shipyard :: Maybe Bool -- ^ Whether it's a shipyard
  , bajoranSettlement :: Maybe Bool -- ^ Whether it's a Bajoran settlement
  , structure :: Maybe Bool -- ^ Whether it's a structure
  }
  deriving (Eq, Show)

locationBaseSchema :: FC.Fleece schema => schema LocationBase
locationBaseSchema =
  FC.object $
    FC.constructor LocationBase
      #+ FC.optionalField FC.OmitKey_DelegateNull "bodyOfWater" bodyOfWater FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "establishment" establishment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "fictionalLocation" fictionalLocation FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "usSettlement" usSettlement FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "country" country FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "colony" colony FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "ds9Establishment" ds9Establishment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "road" road FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "medicalEstablishment" medicalEstablishment FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "earthlyLocation" earthlyLocation FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "mirror" mirror FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "settlement" settlement FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "buildingInterior" buildingInterior FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "subnationalEntity" subnationalEntity FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "landform" landform FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "school" school FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "landmark" landmark FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "religiousLocation" religiousLocation FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "geographicalLocation" geographicalLocation FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "shipyard" shipyard FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "bajoranSettlement" bajoranSettlement FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "structure" structure FC.boolean