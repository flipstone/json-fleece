{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull
  ( CharacterFull(..)
  , characterFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BloodType as BloodType
import qualified StarTrek.Types.CharacterFull.AlternateReality as AlternateReality
import qualified StarTrek.Types.CharacterFull.DayOfBirth as DayOfBirth
import qualified StarTrek.Types.CharacterFull.DayOfDeath as DayOfDeath
import qualified StarTrek.Types.CharacterFull.Deceased as Deceased
import qualified StarTrek.Types.CharacterFull.FictionalCharacter as FictionalCharacter
import qualified StarTrek.Types.CharacterFull.Height as Height
import qualified StarTrek.Types.CharacterFull.Hologram as Hologram
import qualified StarTrek.Types.CharacterFull.HologramActivationDate as HologramActivationDate
import qualified StarTrek.Types.CharacterFull.HologramDateStatus as HologramDateStatus
import qualified StarTrek.Types.CharacterFull.HologramStatus as HologramStatus
import qualified StarTrek.Types.CharacterFull.Mirror as Mirror
import qualified StarTrek.Types.CharacterFull.MonthOfBirth as MonthOfBirth
import qualified StarTrek.Types.CharacterFull.MonthOfDeath as MonthOfDeath
import qualified StarTrek.Types.CharacterFull.Name as Name
import qualified StarTrek.Types.CharacterFull.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.Types.CharacterFull.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.Types.CharacterFull.SerialNumber as SerialNumber
import qualified StarTrek.Types.CharacterFull.Uid as Uid
import qualified StarTrek.Types.CharacterFull.Weight as Weight
import qualified StarTrek.Types.CharacterFull.YearOfBirth as YearOfBirth
import qualified StarTrek.Types.CharacterFull.YearOfDeath as YearOfDeath
import qualified StarTrek.Types.CharacterRelation as CharacterRelation
import qualified StarTrek.Types.CharacterSpecies as CharacterSpecies
import qualified StarTrek.Types.EpisodeBase as EpisodeBase
import qualified StarTrek.Types.Gender as Gender
import qualified StarTrek.Types.MaritalStatus as MaritalStatus
import qualified StarTrek.Types.MovieBase as MovieBase
import qualified StarTrek.Types.OccupationBase as OccupationBase
import qualified StarTrek.Types.OrganizationBase as OrganizationBase
import qualified StarTrek.Types.PerformerBase as PerformerBase
import qualified StarTrek.Types.TitleBase as TitleBase

data CharacterFull = CharacterFull
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this character is from alternate reality
  , fictionalCharacter :: Maybe FictionalCharacter.FictionalCharacter -- ^ Whether this character is a fictional character (from universe point of view)
  , yearOfDeath :: Maybe YearOfDeath.YearOfDeath -- ^ Year the character died
  , name :: Name.Name -- ^ Character name
  , deceased :: Maybe Deceased.Deceased -- ^ Whether this character is deceased
  , maritalStatus :: Maybe MaritalStatus.MaritalStatus -- ^ Marital status
  , dayOfBirth :: Maybe DayOfBirth.DayOfBirth -- ^ Day the character was born
  , performers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , monthOfBirth :: Maybe MonthOfBirth.MonthOfBirth -- ^ Month the character was born
  , occupations :: Maybe [OccupationBase.OccupationBase] -- ^ Base occupations, returned in search results
  , bloodType :: Maybe BloodType.BloodType -- ^ Blood type
  , uid :: Uid.Uid -- ^ Character unique ID
  , monthOfDeath :: Maybe MonthOfDeath.MonthOfDeath -- ^ Month the character died
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this character is from mirror universe
  , hologramDateStatus :: Maybe HologramDateStatus.HologramDateStatus -- ^ Hologram date status
  , hologram :: Maybe Hologram.Hologram -- ^ Whether this character is a hologram
  , gender :: Maybe Gender.Gender -- ^ Gender
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place of death
  , serialNumber :: Maybe SerialNumber.SerialNumber -- ^ Serial number
  , movies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , titles :: Maybe [TitleBase.TitleBase] -- ^ Base title, returned in search results
  , characterRelations :: Maybe [CharacterRelation.CharacterRelation] -- ^ Relation between characters
  , weight :: Maybe Weight.Weight -- ^ Weight in kilograms
  , dayOfDeath :: Maybe DayOfDeath.DayOfDeath -- ^ Day the character died
  , organizations :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , characterSpecies :: Maybe [CharacterSpecies.CharacterSpecies] -- ^ Species a character belongs to
  , hologramActivationDate :: Maybe HologramActivationDate.HologramActivationDate -- ^ Hologram activation date
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place of birth
  , height :: Maybe Height.Height -- ^ Height in centimeters
  , yearOfBirth :: Maybe YearOfBirth.YearOfBirth -- ^ Year the character was born
  , hologramStatus :: Maybe HologramStatus.HologramStatus -- ^ Hologram status
  }
  deriving (Eq, Show)

characterFullSchema :: FC.Fleece schema => schema CharacterFull
characterFullSchema =
  FC.object $
    FC.constructor CharacterFull
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "fictionalCharacter" fictionalCharacter FictionalCharacter.fictionalCharacterSchema
      #+ FC.optional "yearOfDeath" yearOfDeath YearOfDeath.yearOfDeathSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "deceased" deceased Deceased.deceasedSchema
      #+ FC.optional "maritalStatus" maritalStatus MaritalStatus.maritalStatusSchema
      #+ FC.optional "dayOfBirth" dayOfBirth DayOfBirth.dayOfBirthSchema
      #+ FC.optional "performers" performers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "monthOfBirth" monthOfBirth MonthOfBirth.monthOfBirthSchema
      #+ FC.optional "occupations" occupations (FC.list OccupationBase.occupationBaseSchema)
      #+ FC.optional "bloodType" bloodType BloodType.bloodTypeSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "monthOfDeath" monthOfDeath MonthOfDeath.monthOfDeathSchema
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "hologramDateStatus" hologramDateStatus HologramDateStatus.hologramDateStatusSchema
      #+ FC.optional "hologram" hologram Hologram.hologramSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "serialNumber" serialNumber SerialNumber.serialNumberSchema
      #+ FC.optional "movies" movies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "titles" titles (FC.list TitleBase.titleBaseSchema)
      #+ FC.optional "characterRelations" characterRelations (FC.list CharacterRelation.characterRelationSchema)
      #+ FC.optional "weight" weight Weight.weightSchema
      #+ FC.optional "dayOfDeath" dayOfDeath DayOfDeath.dayOfDeathSchema
      #+ FC.optional "organizations" organizations (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.optional "characterSpecies" characterSpecies (FC.list CharacterSpecies.characterSpeciesSchema)
      #+ FC.optional "hologramActivationDate" hologramActivationDate HologramActivationDate.hologramActivationDateSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "height" height Height.heightSchema
      #+ FC.optional "yearOfBirth" yearOfBirth YearOfBirth.yearOfBirthSchema
      #+ FC.optional "hologramStatus" hologramStatus HologramStatus.hologramStatusSchema