{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase
  ( CharacterBase(..)
  , characterBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BloodType (BloodType, bloodTypeSchema)
import StarTrek.CharacterBase.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.CharacterBase.DayOfBirth (DayOfBirth, dayOfBirthSchema)
import StarTrek.CharacterBase.DayOfDeath (DayOfDeath, dayOfDeathSchema)
import StarTrek.CharacterBase.Deceased (Deceased, deceasedSchema)
import StarTrek.CharacterBase.FictionalCharacter (FictionalCharacter, fictionalCharacterSchema)
import StarTrek.CharacterBase.Height (Height, heightSchema)
import StarTrek.CharacterBase.Hologram (Hologram, hologramSchema)
import StarTrek.CharacterBase.HologramActivationDate (HologramActivationDate, hologramActivationDateSchema)
import StarTrek.CharacterBase.HologramDateStatus (HologramDateStatus, hologramDateStatusSchema)
import StarTrek.CharacterBase.HologramStatus (HologramStatus, hologramStatusSchema)
import StarTrek.CharacterBase.Mirror (Mirror, mirrorSchema)
import StarTrek.CharacterBase.MonthOfBirth (MonthOfBirth, monthOfBirthSchema)
import StarTrek.CharacterBase.MonthOfDeath (MonthOfDeath, monthOfDeathSchema)
import StarTrek.CharacterBase.Name (Name, nameSchema)
import StarTrek.CharacterBase.PlaceOfBirth (PlaceOfBirth, placeOfBirthSchema)
import StarTrek.CharacterBase.PlaceOfDeath (PlaceOfDeath, placeOfDeathSchema)
import StarTrek.CharacterBase.SerialNumber (SerialNumber, serialNumberSchema)
import StarTrek.CharacterBase.Uid (Uid, uidSchema)
import StarTrek.CharacterBase.Weight (Weight, weightSchema)
import StarTrek.CharacterBase.YearOfBirth (YearOfBirth, yearOfBirthSchema)
import StarTrek.CharacterBase.YearOfDeath (YearOfDeath, yearOfDeathSchema)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.MaritalStatus (MaritalStatus, maritalStatusSchema)

data CharacterBase = CharacterBase
  { alternateReality :: Maybe AlternateReality -- ^ Whether this character is from alternate reality
  , fictionalCharacter :: Maybe FictionalCharacter -- ^ Whether this character is a fictional character (from universe point of view)
  , yearOfDeath :: Maybe YearOfDeath -- ^ Year the character died
  , name :: Name -- ^ Character name
  , deceased :: Maybe Deceased -- ^ Whether this character is deceased
  , maritalStatus :: Maybe MaritalStatus -- ^ Marital status
  , dayOfBirth :: Maybe DayOfBirth -- ^ Day the character was born
  , monthOfBirth :: Maybe MonthOfBirth -- ^ Month the character was born
  , bloodType :: Maybe BloodType -- ^ Blood type
  , uid :: Uid -- ^ Character unique ID
  , monthOfDeath :: Maybe MonthOfDeath -- ^ Month the character died
  , mirror :: Maybe Mirror -- ^ Whether this character is from mirror universe
  , hologramDateStatus :: Maybe HologramDateStatus -- ^ Hologram date status
  , hologram :: Maybe Hologram -- ^ Whether this character is a hologram
  , gender :: Maybe Gender -- ^ Gender
  , placeOfDeath :: Maybe PlaceOfDeath -- ^ Place of death
  , serialNumber :: Maybe SerialNumber -- ^ Serial number
  , weight :: Maybe Weight -- ^ Weight in kilograms
  , dayOfDeath :: Maybe DayOfDeath -- ^ Day the character died
  , hologramActivationDate :: Maybe HologramActivationDate -- ^ Hologram activation date
  , placeOfBirth :: Maybe PlaceOfBirth -- ^ Place of birth
  , height :: Maybe Height -- ^ Height in centimeters
  , yearOfBirth :: Maybe YearOfBirth -- ^ Year the character was born
  , hologramStatus :: Maybe HologramStatus -- ^ Hologram status
  }
  deriving (Eq, Show)

characterBaseSchema :: FC.Fleece schema => schema CharacterBase
characterBaseSchema =
  FC.object $
    FC.constructor CharacterBase
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.optional "fictionalCharacter" fictionalCharacter fictionalCharacterSchema
      #+ FC.optional "yearOfDeath" yearOfDeath yearOfDeathSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "deceased" deceased deceasedSchema
      #+ FC.optional "maritalStatus" maritalStatus maritalStatusSchema
      #+ FC.optional "dayOfBirth" dayOfBirth dayOfBirthSchema
      #+ FC.optional "monthOfBirth" monthOfBirth monthOfBirthSchema
      #+ FC.optional "bloodType" bloodType bloodTypeSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "monthOfDeath" monthOfDeath monthOfDeathSchema
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "hologramDateStatus" hologramDateStatus hologramDateStatusSchema
      #+ FC.optional "hologram" hologram hologramSchema
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "placeOfDeath" placeOfDeath placeOfDeathSchema
      #+ FC.optional "serialNumber" serialNumber serialNumberSchema
      #+ FC.optional "weight" weight weightSchema
      #+ FC.optional "dayOfDeath" dayOfDeath dayOfDeathSchema
      #+ FC.optional "hologramActivationDate" hologramActivationDate hologramActivationDateSchema
      #+ FC.optional "placeOfBirth" placeOfBirth placeOfBirthSchema
      #+ FC.optional "height" height heightSchema
      #+ FC.optional "yearOfBirth" yearOfBirth yearOfBirthSchema
      #+ FC.optional "hologramStatus" hologramStatus hologramStatusSchema