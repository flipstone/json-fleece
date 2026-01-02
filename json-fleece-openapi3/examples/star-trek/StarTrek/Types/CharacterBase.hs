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
  , bloodType :: Maybe BloodType.BloodType -- ^ Blood type
  , dayOfBirth :: Maybe DayOfBirth.DayOfBirth -- ^ Day the character was born
  , dayOfDeath :: Maybe DayOfDeath.DayOfDeath -- ^ Day the character died
  , deceased :: Maybe Deceased.Deceased -- ^ Whether this character is deceased
  , fictionalCharacter :: Maybe FictionalCharacter.FictionalCharacter -- ^ Whether this character is a fictional character (from universe point of view)
  , gender :: Maybe Gender.Gender -- ^ Gender
  , height :: Maybe Height.Height -- ^ Height in centimeters
  , hologram :: Maybe Hologram.Hologram -- ^ Whether this character is a hologram
  , hologramActivationDate :: Maybe HologramActivationDate.HologramActivationDate -- ^ Hologram activation date
  , hologramDateStatus :: Maybe HologramDateStatus.HologramDateStatus -- ^ Hologram date status
  , hologramStatus :: Maybe HologramStatus.HologramStatus -- ^ Hologram status
  , maritalStatus :: Maybe MaritalStatus.MaritalStatus -- ^ Marital status
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this character is from mirror universe
  , monthOfBirth :: Maybe MonthOfBirth.MonthOfBirth -- ^ Month the character was born
  , monthOfDeath :: Maybe MonthOfDeath.MonthOfDeath -- ^ Month the character died
  , name :: Name.Name -- ^ Character name
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place of birth
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place of death
  , serialNumber :: Maybe SerialNumber.SerialNumber -- ^ Serial number
  , uid :: Uid.Uid -- ^ Character unique ID
  , weight :: Maybe Weight.Weight -- ^ Weight in kilograms
  , yearOfBirth :: Maybe YearOfBirth.YearOfBirth -- ^ Year the character was born
  , yearOfDeath :: Maybe YearOfDeath.YearOfDeath -- ^ Year the character died
  }
  deriving (Eq, Show)

characterBaseSchema :: FC.Fleece t => FC.Schema t CharacterBase
characterBaseSchema =
  FC.object $
    FC.constructor CharacterBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "bloodType" bloodType BloodType.bloodTypeSchema
      #+ FC.optional "dayOfBirth" dayOfBirth DayOfBirth.dayOfBirthSchema
      #+ FC.optional "dayOfDeath" dayOfDeath DayOfDeath.dayOfDeathSchema
      #+ FC.optional "deceased" deceased Deceased.deceasedSchema
      #+ FC.optional "fictionalCharacter" fictionalCharacter FictionalCharacter.fictionalCharacterSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "height" height Height.heightSchema
      #+ FC.optional "hologram" hologram Hologram.hologramSchema
      #+ FC.optional "hologramActivationDate" hologramActivationDate HologramActivationDate.hologramActivationDateSchema
      #+ FC.optional "hologramDateStatus" hologramDateStatus HologramDateStatus.hologramDateStatusSchema
      #+ FC.optional "hologramStatus" hologramStatus HologramStatus.hologramStatusSchema
      #+ FC.optional "maritalStatus" maritalStatus MaritalStatus.maritalStatusSchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "monthOfBirth" monthOfBirth MonthOfBirth.monthOfBirthSchema
      #+ FC.optional "monthOfDeath" monthOfDeath MonthOfDeath.monthOfDeathSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "serialNumber" serialNumber SerialNumber.serialNumberSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "weight" weight Weight.weightSchema
      #+ FC.optional "yearOfBirth" yearOfBirth YearOfBirth.yearOfBirthSchema
      #+ FC.optional "yearOfDeath" yearOfDeath YearOfDeath.yearOfDeathSchema