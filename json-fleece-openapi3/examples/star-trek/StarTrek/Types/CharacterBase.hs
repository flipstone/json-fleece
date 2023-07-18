{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase
  ( CharacterBase(..)
  , characterBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BloodType as BloodType
import qualified StarTrek.Types.CharacterBase.AlternateReality as AlternateReality
import qualified StarTrek.Types.CharacterBase.DayOfBirth as DayOfBirth
import qualified StarTrek.Types.CharacterBase.DayOfDeath as DayOfDeath
import qualified StarTrek.Types.CharacterBase.Deceased as Deceased
import qualified StarTrek.Types.CharacterBase.FictionalCharacter as FictionalCharacter
import qualified StarTrek.Types.CharacterBase.Height as Height
import qualified StarTrek.Types.CharacterBase.Hologram as Hologram
import qualified StarTrek.Types.CharacterBase.HologramActivationDate as HologramActivationDate
import qualified StarTrek.Types.CharacterBase.HologramDateStatus as HologramDateStatus
import qualified StarTrek.Types.CharacterBase.HologramStatus as HologramStatus
import qualified StarTrek.Types.CharacterBase.Mirror as Mirror
import qualified StarTrek.Types.CharacterBase.MonthOfBirth as MonthOfBirth
import qualified StarTrek.Types.CharacterBase.MonthOfDeath as MonthOfDeath
import qualified StarTrek.Types.CharacterBase.Name as Name
import qualified StarTrek.Types.CharacterBase.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.Types.CharacterBase.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.Types.CharacterBase.SerialNumber as SerialNumber
import qualified StarTrek.Types.CharacterBase.Uid as Uid
import qualified StarTrek.Types.CharacterBase.Weight as Weight
import qualified StarTrek.Types.CharacterBase.YearOfBirth as YearOfBirth
import qualified StarTrek.Types.CharacterBase.YearOfDeath as YearOfDeath
import qualified StarTrek.Types.Gender as Gender
import qualified StarTrek.Types.MaritalStatus as MaritalStatus

data CharacterBase = CharacterBase
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this character is from alternate reality
  , monthOfDeath :: Maybe MonthOfDeath.MonthOfDeath -- ^ Month the character died
  , dayOfDeath :: Maybe DayOfDeath.DayOfDeath -- ^ Day the character died
  , hologram :: Maybe Hologram.Hologram -- ^ Whether this character is a hologram
  , yearOfDeath :: Maybe YearOfDeath.YearOfDeath -- ^ Year the character died
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place of birth
  , serialNumber :: Maybe SerialNumber.SerialNumber -- ^ Serial number
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this character is from mirror universe
  , dayOfBirth :: Maybe DayOfBirth.DayOfBirth -- ^ Day the character was born
  , uid :: Uid.Uid -- ^ Character unique ID
  , hologramActivationDate :: Maybe HologramActivationDate.HologramActivationDate -- ^ Hologram activation date
  , weight :: Maybe Weight.Weight -- ^ Weight in kilograms
  , monthOfBirth :: Maybe MonthOfBirth.MonthOfBirth -- ^ Month the character was born
  , fictionalCharacter :: Maybe FictionalCharacter.FictionalCharacter -- ^ Whether this character is a fictional character (from universe point of view)
  , yearOfBirth :: Maybe YearOfBirth.YearOfBirth -- ^ Year the character was born
  , deceased :: Maybe Deceased.Deceased -- ^ Whether this character is deceased
  , gender :: Maybe Gender.Gender -- ^ Gender
  , hologramDateStatus :: Maybe HologramDateStatus.HologramDateStatus -- ^ Hologram date status
  , maritalStatus :: Maybe MaritalStatus.MaritalStatus -- ^ Marital status
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place of death
  , height :: Maybe Height.Height -- ^ Height in centimeters
  , name :: Name.Name -- ^ Character name
  , hologramStatus :: Maybe HologramStatus.HologramStatus -- ^ Hologram status
  , bloodType :: Maybe BloodType.BloodType -- ^ Blood type
  }
  deriving (Eq, Show)

characterBaseSchema :: FC.Fleece schema => schema CharacterBase
characterBaseSchema =
  FC.object $
    FC.constructor CharacterBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "monthOfDeath" monthOfDeath MonthOfDeath.monthOfDeathSchema
      #+ FC.optional "dayOfDeath" dayOfDeath DayOfDeath.dayOfDeathSchema
      #+ FC.optional "hologram" hologram Hologram.hologramSchema
      #+ FC.optional "yearOfDeath" yearOfDeath YearOfDeath.yearOfDeathSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "serialNumber" serialNumber SerialNumber.serialNumberSchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "dayOfBirth" dayOfBirth DayOfBirth.dayOfBirthSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "hologramActivationDate" hologramActivationDate HologramActivationDate.hologramActivationDateSchema
      #+ FC.optional "weight" weight Weight.weightSchema
      #+ FC.optional "monthOfBirth" monthOfBirth MonthOfBirth.monthOfBirthSchema
      #+ FC.optional "fictionalCharacter" fictionalCharacter FictionalCharacter.fictionalCharacterSchema
      #+ FC.optional "yearOfBirth" yearOfBirth YearOfBirth.yearOfBirthSchema
      #+ FC.optional "deceased" deceased Deceased.deceasedSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "hologramDateStatus" hologramDateStatus HologramDateStatus.hologramDateStatusSchema
      #+ FC.optional "maritalStatus" maritalStatus MaritalStatus.maritalStatusSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "height" height Height.heightSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "hologramStatus" hologramStatus HologramStatus.hologramStatusSchema
      #+ FC.optional "bloodType" bloodType BloodType.bloodTypeSchema