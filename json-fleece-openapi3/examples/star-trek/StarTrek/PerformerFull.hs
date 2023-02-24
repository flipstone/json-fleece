{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFull
  ( PerformerFull(..)
  , performerFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.MovieBase (MovieBase, movieBaseSchema)

data PerformerFull = PerformerFull
  { birthName :: Maybe Text -- ^ Performer birth name
  , name :: Text -- ^ Performer name
  , moviesStandInPerformances :: Maybe [MovieBase] -- ^ Movies in which this person appeared as a stand-in performer
  , tngPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , animalPerformer :: Maybe Bool -- ^ Whether it's an animal performer
  , standInPerformer :: Maybe Bool -- ^ Whether it's a stand-in performer
  , episodesStandInPerformances :: Maybe [EpisodeBase] -- ^ Episodes in which this person appeared as a stand-in performer
  , tasPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , uid :: Text -- ^ Performer unique ID
  , characters :: Maybe [CharacterBase] -- ^ Characters played by this performer
  , dateOfDeath :: Maybe Text -- ^ Date the performer died
  , stuntPerformer :: Maybe Bool -- ^ Whether it's a stunt performer
  , moviesStuntPerformances :: Maybe [MovieBase] -- ^ Movies in which this person appeared as a stunt performer
  , entPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , episodesStuntPerformances :: Maybe [EpisodeBase] -- ^ Episodes in which this person appeared as a stunt performer
  , voicePerformer :: Maybe Bool -- ^ Whether it's a voice performer
  , disPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , ds9Performer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , gender :: Maybe Gender -- ^ Gender
  , dateOfBirth :: Maybe Text -- ^ Date the performer was born
  , moviesPerformances :: Maybe [MovieBase] -- ^ Movies in which this person appeared as a performer
  , placeOfDeath :: Maybe Text -- ^ Place the performer died
  , tosPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , episodesPerformances :: Maybe [EpisodeBase] -- ^ Episodes in which this person appeared as a performer
  , videoGamePerformer :: Maybe Bool -- ^ Whether it's a video game performer
  , filmPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in a Star Trek movie
  , placeOfBirth :: Maybe Text -- ^ Place the performer was born
  , voyPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  }
  deriving (Eq, Show)

performerFullSchema :: FC.Fleece schema => schema PerformerFull
performerFullSchema =
  FC.object $
    FC.constructor PerformerFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "birthName" birthName FC.text
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "moviesStandInPerformances" moviesStandInPerformances (FC.list movieBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "tngPerformer" tngPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "animalPerformer" animalPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "standInPerformer" standInPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "episodesStandInPerformances" episodesStandInPerformances (FC.list episodeBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "tasPerformer" tasPerformer FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "dateOfDeath" dateOfDeath FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stuntPerformer" stuntPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "moviesStuntPerformances" moviesStuntPerformances (FC.list movieBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "entPerformer" entPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "episodesStuntPerformances" episodesStuntPerformances (FC.list episodeBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "voicePerformer" voicePerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "disPerformer" disPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "ds9Performer" ds9Performer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "gender" gender genderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "dateOfBirth" dateOfBirth FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "moviesPerformances" moviesPerformances (FC.list movieBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "placeOfDeath" placeOfDeath FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "tosPerformer" tosPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "episodesPerformances" episodesPerformances (FC.list episodeBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "videoGamePerformer" videoGamePerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "filmPerformer" filmPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "placeOfBirth" placeOfBirth FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "voyPerformer" voyPerformer FC.boolean