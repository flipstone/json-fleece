{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull
  ( LocationFull(..)
  , locationFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data LocationFull = LocationFull
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

locationFullSchema :: FC.Fleece schema => schema LocationFull
locationFullSchema =
  FC.object $
    FC.constructor LocationFull
      #+ FC.optional "bodyOfWater" bodyOfWater FC.boolean
      #+ FC.optional "establishment" establishment FC.boolean
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "fictionalLocation" fictionalLocation FC.boolean
      #+ FC.optional "usSettlement" usSettlement FC.boolean
      #+ FC.optional "country" country FC.boolean
      #+ FC.optional "colony" colony FC.boolean
      #+ FC.optional "ds9Establishment" ds9Establishment FC.boolean
      #+ FC.optional "road" road FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "medicalEstablishment" medicalEstablishment FC.boolean
      #+ FC.optional "earthlyLocation" earthlyLocation FC.boolean
      #+ FC.optional "mirror" mirror FC.boolean
      #+ FC.optional "settlement" settlement FC.boolean
      #+ FC.optional "buildingInterior" buildingInterior FC.boolean
      #+ FC.optional "subnationalEntity" subnationalEntity FC.boolean
      #+ FC.optional "landform" landform FC.boolean
      #+ FC.optional "school" school FC.boolean
      #+ FC.optional "landmark" landmark FC.boolean
      #+ FC.optional "religiousLocation" religiousLocation FC.boolean
      #+ FC.optional "geographicalLocation" geographicalLocation FC.boolean
      #+ FC.optional "shipyard" shipyard FC.boolean
      #+ FC.optional "bajoranSettlement" bajoranSettlement FC.boolean
      #+ FC.optional "structure" structure FC.boolean