{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase
  ( CharacterBase(..)
  , characterBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.BloodType (BloodType, bloodTypeSchema)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.MaritalStatus (MaritalStatus, maritalStatusSchema)

data CharacterBase = CharacterBase
  { alternateReality :: Maybe Bool -- ^ Whether this character is from alternate reality
  , fictionalCharacter :: Maybe Bool -- ^ Whether this character is a fictional character (from universe point of view)
  , yearOfDeath :: Maybe Integer -- ^ Year the character died
  , name :: Text -- ^ Character name
  , deceased :: Maybe Bool -- ^ Whether this character is deceased
  , maritalStatus :: Maybe MaritalStatus -- ^ Marital status
  , dayOfBirth :: Maybe Integer -- ^ Day the character was born
  , monthOfBirth :: Maybe Integer -- ^ Month the character was born
  , bloodType :: Maybe BloodType -- ^ Blood type
  , uid :: Text -- ^ Character unique ID
  , monthOfDeath :: Maybe Integer -- ^ Month the character died
  , mirror :: Maybe Bool -- ^ Whether this character is from mirror universe
  , hologramDateStatus :: Maybe Text -- ^ Hologram date status
  , hologram :: Maybe Bool -- ^ Whether this character is a hologram
  , gender :: Maybe Gender -- ^ Gender
  , placeOfDeath :: Maybe Text -- ^ Place of death
  , serialNumber :: Maybe Text -- ^ Serial number
  , weight :: Maybe Integer -- ^ Weight in kilograms
  , dayOfDeath :: Maybe Integer -- ^ Day the character died
  , hologramActivationDate :: Maybe Text -- ^ Hologram activation date
  , placeOfBirth :: Maybe Text -- ^ Place of birth
  , height :: Maybe Integer -- ^ Height in centimeters
  , yearOfBirth :: Maybe Integer -- ^ Year the character was born
  , hologramStatus :: Maybe Text -- ^ Hologram status
  }
  deriving (Eq, Show)

characterBaseSchema :: FC.Fleece schema => schema CharacterBase
characterBaseSchema =
  FC.object $
    FC.constructor CharacterBase
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.optional "fictionalCharacter" fictionalCharacter FC.boolean
      #+ FC.optional "yearOfDeath" yearOfDeath FC.integer
      #+ FC.required "name" name FC.text
      #+ FC.optional "deceased" deceased FC.boolean
      #+ FC.optional "maritalStatus" maritalStatus maritalStatusSchema
      #+ FC.optional "dayOfBirth" dayOfBirth FC.integer
      #+ FC.optional "monthOfBirth" monthOfBirth FC.integer
      #+ FC.optional "bloodType" bloodType bloodTypeSchema
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "monthOfDeath" monthOfDeath FC.integer
      #+ FC.optional "mirror" mirror FC.boolean
      #+ FC.optional "hologramDateStatus" hologramDateStatus FC.text
      #+ FC.optional "hologram" hologram FC.boolean
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "placeOfDeath" placeOfDeath FC.text
      #+ FC.optional "serialNumber" serialNumber FC.text
      #+ FC.optional "weight" weight FC.integer
      #+ FC.optional "dayOfDeath" dayOfDeath FC.integer
      #+ FC.optional "hologramActivationDate" hologramActivationDate FC.text
      #+ FC.optional "placeOfBirth" placeOfBirth FC.text
      #+ FC.optional "height" height FC.integer
      #+ FC.optional "yearOfBirth" yearOfBirth FC.integer
      #+ FC.optional "hologramStatus" hologramStatus FC.text