{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull
  ( CharacterFull(..)
  , characterFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Integer, Maybe, Show)
import StarTrek.BloodType (BloodType, bloodTypeSchema)
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
  { alternateReality :: Maybe Bool -- ^ Whether this character is from alternate reality
  , fictionalCharacter :: Maybe Bool -- ^ Whether this character is a fictional character (from universe point of view)
  , yearOfDeath :: Maybe Integer -- ^ Year the character died
  , name :: Text -- ^ Character name
  , deceased :: Maybe Bool -- ^ Whether this character is deceased
  , maritalStatus :: Maybe MaritalStatus -- ^ Marital status
  , dayOfBirth :: Maybe Integer -- ^ Day the character was born
  , performers :: Maybe [PerformerBase] -- ^ Performers who played this character
  , monthOfBirth :: Maybe Integer -- ^ Month the character was born
  , occupations :: Maybe [OccupationBase] -- ^ Occupations of this character
  , bloodType :: Maybe BloodType -- ^ Blood type
  , uid :: Text -- ^ Character unique ID
  , monthOfDeath :: Maybe Integer -- ^ Month the character died
  , episodes :: Maybe [EpisodeBase] -- ^ Episodes in which this character appeared
  , mirror :: Maybe Bool -- ^ Whether this character is from mirror universe
  , hologramDateStatus :: Maybe Text -- ^ Hologram date status
  , hologram :: Maybe Bool -- ^ Whether this character is a hologram
  , gender :: Maybe Gender -- ^ Gender
  , placeOfDeath :: Maybe Text -- ^ Place of death
  , serialNumber :: Maybe Text -- ^ Serial number
  , movies :: Maybe [MovieBase] -- ^ Movies in which this character appeared
  , titles :: Maybe [TitleBase] -- ^ Titles this character holds
  , characterRelations :: Maybe [CharacterRelation] -- ^ Relations with other characters
  , weight :: Maybe Integer -- ^ Weight in kilograms
  , dayOfDeath :: Maybe Integer -- ^ Day the character died
  , organizations :: Maybe [OrganizationBase] -- ^ Organizations this character has affiliation with
  , characterSpecies :: Maybe [CharacterSpecies] -- ^ Species this character belongs to
  , hologramActivationDate :: Maybe Text -- ^ Hologram activation date
  , placeOfBirth :: Maybe Text -- ^ Place of birth
  , height :: Maybe Integer -- ^ Height in centimeters
  , yearOfBirth :: Maybe Integer -- ^ Year the character was born
  , hologramStatus :: Maybe Text -- ^ Hologram status
  }
  deriving (Eq, Show)

characterFullSchema :: FC.Fleece schema => schema CharacterFull
characterFullSchema =
  FC.object $
    FC.constructor CharacterFull
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.optional "fictionalCharacter" fictionalCharacter FC.boolean
      #+ FC.optional "yearOfDeath" yearOfDeath FC.integer
      #+ FC.required "name" name FC.text
      #+ FC.optional "deceased" deceased FC.boolean
      #+ FC.optional "maritalStatus" maritalStatus maritalStatusSchema
      #+ FC.optional "dayOfBirth" dayOfBirth FC.integer
      #+ FC.optional "performers" performers (FC.list performerBaseSchema)
      #+ FC.optional "monthOfBirth" monthOfBirth FC.integer
      #+ FC.optional "occupations" occupations (FC.list occupationBaseSchema)
      #+ FC.optional "bloodType" bloodType bloodTypeSchema
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "monthOfDeath" monthOfDeath FC.integer
      #+ FC.optional "episodes" episodes (FC.list episodeBaseSchema)
      #+ FC.optional "mirror" mirror FC.boolean
      #+ FC.optional "hologramDateStatus" hologramDateStatus FC.text
      #+ FC.optional "hologram" hologram FC.boolean
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "placeOfDeath" placeOfDeath FC.text
      #+ FC.optional "serialNumber" serialNumber FC.text
      #+ FC.optional "movies" movies (FC.list movieBaseSchema)
      #+ FC.optional "titles" titles (FC.list titleBaseSchema)
      #+ FC.optional "characterRelations" characterRelations (FC.list characterRelationSchema)
      #+ FC.optional "weight" weight FC.integer
      #+ FC.optional "dayOfDeath" dayOfDeath FC.integer
      #+ FC.optional "organizations" organizations (FC.list organizationBaseSchema)
      #+ FC.optional "characterSpecies" characterSpecies (FC.list characterSpeciesSchema)
      #+ FC.optional "hologramActivationDate" hologramActivationDate FC.text
      #+ FC.optional "placeOfBirth" placeOfBirth FC.text
      #+ FC.optional "height" height FC.integer
      #+ FC.optional "yearOfBirth" yearOfBirth FC.integer
      #+ FC.optional "hologramStatus" hologramStatus FC.text