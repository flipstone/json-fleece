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
  , movies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , monthOfDeath :: Maybe MonthOfDeath.MonthOfDeath -- ^ Month the character died
  , dayOfDeath :: Maybe DayOfDeath.DayOfDeath -- ^ Day the character died
  , characterRelations :: Maybe [CharacterRelation.CharacterRelation] -- ^ Relation between characters
  , hologram :: Maybe Hologram.Hologram -- ^ Whether this character is a hologram
  , yearOfDeath :: Maybe YearOfDeath.YearOfDeath -- ^ Year the character died
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place of birth
  , serialNumber :: Maybe SerialNumber.SerialNumber -- ^ Serial number
  , titles :: Maybe [TitleBase.TitleBase] -- ^ Base title, returned in search results
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this character is from mirror universe
  , dayOfBirth :: Maybe DayOfBirth.DayOfBirth -- ^ Day the character was born
  , organizations :: Maybe [OrganizationBase.OrganizationBase] -- ^ Base organization, returned in search results
  , uid :: Uid.Uid -- ^ Character unique ID
  , performers :: Maybe [PerformerBase.PerformerBase] -- ^ Base performer, returned in search results
  , hologramActivationDate :: Maybe HologramActivationDate.HologramActivationDate -- ^ Hologram activation date
  , weight :: Maybe Weight.Weight -- ^ Weight in kilograms
  , monthOfBirth :: Maybe MonthOfBirth.MonthOfBirth -- ^ Month the character was born
  , occupations :: Maybe [OccupationBase.OccupationBase] -- ^ Base occupations, returned in search results
  , episodes :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , fictionalCharacter :: Maybe FictionalCharacter.FictionalCharacter -- ^ Whether this character is a fictional character (from universe point of view)
  , characterSpecies :: Maybe [CharacterSpecies.CharacterSpecies] -- ^ Species a character belongs to
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

characterFullSchema :: FC.Fleece schema => schema CharacterFull
characterFullSchema =
  FC.object $
    FC.constructor CharacterFull
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "movies" movies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "monthOfDeath" monthOfDeath MonthOfDeath.monthOfDeathSchema
      #+ FC.optional "dayOfDeath" dayOfDeath DayOfDeath.dayOfDeathSchema
      #+ FC.optional "characterRelations" characterRelations (FC.list CharacterRelation.characterRelationSchema)
      #+ FC.optional "hologram" hologram Hologram.hologramSchema
      #+ FC.optional "yearOfDeath" yearOfDeath YearOfDeath.yearOfDeathSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "serialNumber" serialNumber SerialNumber.serialNumberSchema
      #+ FC.optional "titles" titles (FC.list TitleBase.titleBaseSchema)
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.optional "dayOfBirth" dayOfBirth DayOfBirth.dayOfBirthSchema
      #+ FC.optional "organizations" organizations (FC.list OrganizationBase.organizationBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "performers" performers (FC.list PerformerBase.performerBaseSchema)
      #+ FC.optional "hologramActivationDate" hologramActivationDate HologramActivationDate.hologramActivationDateSchema
      #+ FC.optional "weight" weight Weight.weightSchema
      #+ FC.optional "monthOfBirth" monthOfBirth MonthOfBirth.monthOfBirthSchema
      #+ FC.optional "occupations" occupations (FC.list OccupationBase.occupationBaseSchema)
      #+ FC.optional "episodes" episodes (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "fictionalCharacter" fictionalCharacter FictionalCharacter.fictionalCharacterSchema
      #+ FC.optional "characterSpecies" characterSpecies (FC.list CharacterSpecies.characterSpeciesSchema)
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