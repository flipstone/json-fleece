{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase
  ( CharacterBase(..)
  , characterBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BloodType as BloodType
import qualified StarTrek.CharacterBase.AlternateReality as AlternateReality
import qualified StarTrek.CharacterBase.DayOfBirth as DayOfBirth
import qualified StarTrek.CharacterBase.DayOfDeath as DayOfDeath
import qualified StarTrek.CharacterBase.Deceased as Deceased
import qualified StarTrek.CharacterBase.FictionalCharacter as FictionalCharacter
import qualified StarTrek.CharacterBase.Height as Height
import qualified StarTrek.CharacterBase.Hologram as Hologram
import qualified StarTrek.CharacterBase.HologramActivationDate as HologramActivationDate
import qualified StarTrek.CharacterBase.HologramDateStatus as HologramDateStatus
import qualified StarTrek.CharacterBase.HologramStatus as HologramStatus
import qualified StarTrek.CharacterBase.Mirror as Mirror
import qualified StarTrek.CharacterBase.MonthOfBirth as MonthOfBirth
import qualified StarTrek.CharacterBase.MonthOfDeath as MonthOfDeath
import qualified StarTrek.CharacterBase.Name as Name
import qualified StarTrek.CharacterBase.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.CharacterBase.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.CharacterBase.SerialNumber as SerialNumber
import qualified StarTrek.CharacterBase.Uid as Uid
import qualified StarTrek.CharacterBase.Weight as Weight
import qualified StarTrek.CharacterBase.YearOfBirth as YearOfBirth
import qualified StarTrek.CharacterBase.YearOfDeath as YearOfDeath
import qualified StarTrek.Gender as Gender
import qualified StarTrek.MaritalStatus as MaritalStatus

data CharacterBase = CharacterBase
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this character is from alternate reality
  , fictionalCharacter :: Maybe FictionalCharacter.FictionalCharacter -- ^ Whether this character is a fictional character (from universe point of view)
  , yearOfDeath :: Maybe YearOfDeath.YearOfDeath -- ^ Year the character died
  , name :: Name.Name -- ^ Character name
  , deceased :: Maybe Deceased.Deceased -- ^ Whether this character is deceased
  , maritalStatus :: Maybe MaritalStatus.MaritalStatus -- ^ Marital status
  , dayOfBirth :: Maybe DayOfBirth.DayOfBirth -- ^ Day the character was born
  , monthOfBirth :: Maybe MonthOfBirth.MonthOfBirth -- ^ Month the character was born
  , bloodType :: Maybe BloodType.BloodType -- ^ Blood type
  , uid :: Uid.Uid -- ^ Character unique ID
  , monthOfDeath :: Maybe MonthOfDeath.MonthOfDeath -- ^ Month the character died
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this character is from mirror universe
  , hologramDateStatus :: Maybe HologramDateStatus.HologramDateStatus -- ^ Hologram date status
  , hologram :: Maybe Hologram.Hologram -- ^ Whether this character is a hologram
  , gender :: Maybe Gender.Gender -- ^ Gender
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place of death
  , serialNumber :: Maybe SerialNumber.SerialNumber -- ^ Serial number
  , weight :: Maybe Weight.Weight -- ^ Weight in kilograms
  , dayOfDeath :: Maybe DayOfDeath.DayOfDeath -- ^ Day the character died
  , hologramActivationDate :: Maybe HologramActivationDate.HologramActivationDate -- ^ Hologram activation date
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place of birth
  , height :: Maybe Height.Height -- ^ Height in centimeters
  , yearOfBirth :: Maybe YearOfBirth.YearOfBirth -- ^ Year the character was born
  , hologramStatus :: Maybe HologramStatus.HologramStatus -- ^ Hologram status
  }
  deriving (Eq, Show)

characterBaseSchema :: FC.Fleece schema => schema CharacterBase
characterBaseSchema =
  FC.object $
    FC.constructor CharacterBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "fictionalCharacter" fictionalCharacter FictionalCharacter.fictionalCharacterSchema
      #+ FC.optional "yearOfDeath" yearOfDeath YearOfDeath.yearOfDeathSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "deceased" deceased Deceased.deceasedSchema
      #+ FC.optional "maritalStatus" maritalStatus MaritalStatus.maritalStatusSchema
      #+ FC.optional "dayOfBirth" dayOfBirth DayOfBirth.dayOfBirthSchema
      #+ FC.optional "monthOfBirth" monthOfBirth MonthOfBirth.monthOfBirthSchema
      #+ FC.optional "bloodType" bloodType BloodType.bloodTypeSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "monthOfDeath" monthOfDeath MonthOfDeath.monthOfDeathSchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "hologramDateStatus" hologramDateStatus HologramDateStatus.hologramDateStatusSchema
      #+ FC.optional "hologram" hologram Hologram.hologramSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "serialNumber" serialNumber SerialNumber.serialNumberSchema
      #+ FC.optional "weight" weight Weight.weightSchema
      #+ FC.optional "dayOfDeath" dayOfDeath DayOfDeath.dayOfDeathSchema
      #+ FC.optional "hologramActivationDate" hologramActivationDate HologramActivationDate.hologramActivationDateSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "height" height Height.heightSchema
      #+ FC.optional "yearOfBirth" yearOfBirth YearOfBirth.yearOfBirthSchema
      #+ FC.optional "hologramStatus" hologramStatus HologramStatus.hologramStatusSchema