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
      #+ FC.optional "birthName" birthName FC.text
      #+ FC.required "name" name FC.text
      #+ FC.optional "moviesStandInPerformances" moviesStandInPerformances (FC.list movieBaseSchema)
      #+ FC.optional "tngPerformer" tngPerformer FC.boolean
      #+ FC.optional "animalPerformer" animalPerformer FC.boolean
      #+ FC.optional "standInPerformer" standInPerformer FC.boolean
      #+ FC.optional "episodesStandInPerformances" episodesStandInPerformances (FC.list episodeBaseSchema)
      #+ FC.optional "tasPerformer" tasPerformer FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "dateOfDeath" dateOfDeath FC.text
      #+ FC.optional "stuntPerformer" stuntPerformer FC.boolean
      #+ FC.optional "moviesStuntPerformances" moviesStuntPerformances (FC.list movieBaseSchema)
      #+ FC.optional "entPerformer" entPerformer FC.boolean
      #+ FC.optional "episodesStuntPerformances" episodesStuntPerformances (FC.list episodeBaseSchema)
      #+ FC.optional "voicePerformer" voicePerformer FC.boolean
      #+ FC.optional "disPerformer" disPerformer FC.boolean
      #+ FC.optional "ds9Performer" ds9Performer FC.boolean
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth FC.text
      #+ FC.optional "moviesPerformances" moviesPerformances (FC.list movieBaseSchema)
      #+ FC.optional "placeOfDeath" placeOfDeath FC.text
      #+ FC.optional "tosPerformer" tosPerformer FC.boolean
      #+ FC.optional "episodesPerformances" episodesPerformances (FC.list episodeBaseSchema)
      #+ FC.optional "videoGamePerformer" videoGamePerformer FC.boolean
      #+ FC.optional "filmPerformer" filmPerformer FC.boolean
      #+ FC.optional "placeOfBirth" placeOfBirth FC.text
      #+ FC.optional "voyPerformer" voyPerformer FC.boolean