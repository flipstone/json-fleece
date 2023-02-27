{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull
  ( CharacterFull(..)
  , characterFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BloodType (BloodType, bloodTypeSchema)
import StarTrek.CharacterFull.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.CharacterFull.DayOfBirth (DayOfBirth, dayOfBirthSchema)
import StarTrek.CharacterFull.DayOfDeath (DayOfDeath, dayOfDeathSchema)
import StarTrek.CharacterFull.Deceased (Deceased, deceasedSchema)
import StarTrek.CharacterFull.FictionalCharacter (FictionalCharacter, fictionalCharacterSchema)
import StarTrek.CharacterFull.Height (Height, heightSchema)
import StarTrek.CharacterFull.Hologram (Hologram, hologramSchema)
import StarTrek.CharacterFull.HologramActivationDate (HologramActivationDate, hologramActivationDateSchema)
import StarTrek.CharacterFull.HologramDateStatus (HologramDateStatus, hologramDateStatusSchema)
import StarTrek.CharacterFull.HologramStatus (HologramStatus, hologramStatusSchema)
import StarTrek.CharacterFull.Mirror (Mirror, mirrorSchema)
import StarTrek.CharacterFull.MonthOfBirth (MonthOfBirth, monthOfBirthSchema)
import StarTrek.CharacterFull.MonthOfDeath (MonthOfDeath, monthOfDeathSchema)
import StarTrek.CharacterFull.Name (Name, nameSchema)
import StarTrek.CharacterFull.PlaceOfBirth (PlaceOfBirth, placeOfBirthSchema)
import StarTrek.CharacterFull.PlaceOfDeath (PlaceOfDeath, placeOfDeathSchema)
import StarTrek.CharacterFull.SerialNumber (SerialNumber, serialNumberSchema)
import StarTrek.CharacterFull.Uid (Uid, uidSchema)
import StarTrek.CharacterFull.Weight (Weight, weightSchema)
import StarTrek.CharacterFull.YearOfBirth (YearOfBirth, yearOfBirthSchema)
import StarTrek.CharacterFull.YearOfDeath (YearOfDeath, yearOfDeathSchema)
import StarTrek.CharacterRelation (CharacterRelation, characterRelationSchema)
import StarTrek.CharacterSpecies (CharacterSpecies, characterSpeciesSchema)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.MaritalStatus (MaritalStatus, maritalStatusSchema)
import StarTrek.MovieBase (MovieBase, movieBaseSchema)
import StarTrek.OccupationBase (OccupationBase, occupationBaseSchema)
import StarTrek.OrganizationBase (OrganizationBase, organizationBaseSchema)
import StarTrek.PerformerBase (PerformerBase, performerBaseSchema)
import StarTrek.TitleBase (TitleBase, titleBaseSchema)

data CharacterFull = CharacterFull
  { alternateReality :: Maybe AlternateReality -- ^ Whether this character is from alternate reality
  , fictionalCharacter :: Maybe FictionalCharacter -- ^ Whether this character is a fictional character (from universe point of view)
  , yearOfDeath :: Maybe YearOfDeath -- ^ Year the character died
  , name :: Name -- ^ Character name
  , deceased :: Maybe Deceased -- ^ Whether this character is deceased
  , maritalStatus :: Maybe MaritalStatus -- ^ Marital status
  , dayOfBirth :: Maybe DayOfBirth -- ^ Day the character was born
  , performers :: Maybe [PerformerBase] -- ^ Base performer, returned in search results
  , monthOfBirth :: Maybe MonthOfBirth -- ^ Month the character was born
  , occupations :: Maybe [OccupationBase] -- ^ Base occupations, returned in search results
  , bloodType :: Maybe BloodType -- ^ Blood type
  , uid :: Uid -- ^ Character unique ID
  , monthOfDeath :: Maybe MonthOfDeath -- ^ Month the character died
  , episodes :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , mirror :: Maybe Mirror -- ^ Whether this character is from mirror universe
  , hologramDateStatus :: Maybe HologramDateStatus -- ^ Hologram date status
  , hologram :: Maybe Hologram -- ^ Whether this character is a hologram
  , gender :: Maybe Gender -- ^ Gender
  , placeOfDeath :: Maybe PlaceOfDeath -- ^ Place of death
  , serialNumber :: Maybe SerialNumber -- ^ Serial number
  , movies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , titles :: Maybe [TitleBase] -- ^ Base title, returned in search results
  , characterRelations :: Maybe [CharacterRelation] -- ^ Relation between characters
  , weight :: Maybe Weight -- ^ Weight in kilograms
  , dayOfDeath :: Maybe DayOfDeath -- ^ Day the character died
  , organizations :: Maybe [OrganizationBase] -- ^ Base organization, returned in search results
  , characterSpecies :: Maybe [CharacterSpecies] -- ^ Species a character belongs to
  , hologramActivationDate :: Maybe HologramActivationDate -- ^ Hologram activation date
  , placeOfBirth :: Maybe PlaceOfBirth -- ^ Place of birth
  , height :: Maybe Height -- ^ Height in centimeters
  , yearOfBirth :: Maybe YearOfBirth -- ^ Year the character was born
  , hologramStatus :: Maybe HologramStatus -- ^ Hologram status
  }
  deriving (Eq, Show)

characterFullSchema :: FC.Fleece schema => schema CharacterFull
characterFullSchema =
  FC.object $
    FC.constructor CharacterFull
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.optional "fictionalCharacter" fictionalCharacter fictionalCharacterSchema
      #+ FC.optional "yearOfDeath" yearOfDeath yearOfDeathSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "deceased" deceased deceasedSchema
      #+ FC.optional "maritalStatus" maritalStatus maritalStatusSchema
      #+ FC.optional "dayOfBirth" dayOfBirth dayOfBirthSchema
      #+ FC.optional "performers" performers (FC.list performerBaseSchema)
      #+ FC.optional "monthOfBirth" monthOfBirth monthOfBirthSchema
      #+ FC.optional "occupations" occupations (FC.list occupationBaseSchema)
      #+ FC.optional "bloodType" bloodType bloodTypeSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "monthOfDeath" monthOfDeath monthOfDeathSchema
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "hologramDateStatus" hologramDateStatus hologramDateStatusSchema
      #+ FC.optional "hologram" hologram hologramSchema
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "placeOfDeath" placeOfDeath placeOfDeathSchema
      #+ FC.optional "serialNumber" serialNumber serialNumberSchema
      #+ FC.optional "movies" movies (FC.list movieBaseSchema)
      #+ FC.optional "titles" titles (FC.list titleBaseSchema)
      #+ FC.optional "characterRelations" characterRelations (FC.list characterRelationSchema)
      #+ FC.optional "weight" weight weightSchema
      #+ FC.optional "dayOfDeath" dayOfDeath dayOfDeathSchema
      #+ FC.optional "organizations" organizations (FC.list organizationBaseSchema)
      #+ FC.optional "characterSpecies" characterSpecies (FC.list characterSpeciesSchema)
      #+ FC.optional "hologramActivationDate" hologramActivationDate hologramActivationDateSchema
      #+ FC.optional "placeOfBirth" placeOfBirth placeOfBirthSchema
      #+ FC.optional "height" height heightSchema
      #+ FC.optional "yearOfBirth" yearOfBirth yearOfBirthSchema
      #+ FC.optional "hologramStatus" hologramStatus hologramStatusSchema