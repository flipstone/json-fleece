{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFull
  ( PerformerFull(..)
  , performerFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.EpisodeBase (EpisodeBase, episodeBaseSchema)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.MovieBase (MovieBase, movieBaseSchema)
import StarTrek.PerformerFull.AnimalPerformer (AnimalPerformer, animalPerformerSchema)
import StarTrek.PerformerFull.BirthName (BirthName, birthNameSchema)
import StarTrek.PerformerFull.DateOfBirth (DateOfBirth, dateOfBirthSchema)
import StarTrek.PerformerFull.DateOfDeath (DateOfDeath, dateOfDeathSchema)
import StarTrek.PerformerFull.DisPerformer (DisPerformer, disPerformerSchema)
import StarTrek.PerformerFull.Ds9Performer (Ds9Performer, ds9PerformerSchema)
import StarTrek.PerformerFull.EntPerformer (EntPerformer, entPerformerSchema)
import StarTrek.PerformerFull.FilmPerformer (FilmPerformer, filmPerformerSchema)
import StarTrek.PerformerFull.Name (Name, nameSchema)
import StarTrek.PerformerFull.PlaceOfBirth (PlaceOfBirth, placeOfBirthSchema)
import StarTrek.PerformerFull.PlaceOfDeath (PlaceOfDeath, placeOfDeathSchema)
import StarTrek.PerformerFull.StandInPerformer (StandInPerformer, standInPerformerSchema)
import StarTrek.PerformerFull.StuntPerformer (StuntPerformer, stuntPerformerSchema)
import StarTrek.PerformerFull.TasPerformer (TasPerformer, tasPerformerSchema)
import StarTrek.PerformerFull.TngPerformer (TngPerformer, tngPerformerSchema)
import StarTrek.PerformerFull.TosPerformer (TosPerformer, tosPerformerSchema)
import StarTrek.PerformerFull.Uid (Uid, uidSchema)
import StarTrek.PerformerFull.VideoGamePerformer (VideoGamePerformer, videoGamePerformerSchema)
import StarTrek.PerformerFull.VoicePerformer (VoicePerformer, voicePerformerSchema)
import StarTrek.PerformerFull.VoyPerformer (VoyPerformer, voyPerformerSchema)

data PerformerFull = PerformerFull
  { birthName :: Maybe BirthName -- ^ Performer birth name
  , name :: Name -- ^ Performer name
  , moviesStandInPerformances :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , tngPerformer :: Maybe TngPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , animalPerformer :: Maybe AnimalPerformer -- ^ Whether it's an animal performer
  , standInPerformer :: Maybe StandInPerformer -- ^ Whether it's a stand-in performer
  , episodesStandInPerformances :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , tasPerformer :: Maybe TasPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , uid :: Uid -- ^ Performer unique ID
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , dateOfDeath :: Maybe DateOfDeath -- ^ Date the performer died
  , stuntPerformer :: Maybe StuntPerformer -- ^ Whether it's a stunt performer
  , moviesStuntPerformances :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , entPerformer :: Maybe EntPerformer -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , episodesStuntPerformances :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , voicePerformer :: Maybe VoicePerformer -- ^ Whether it's a voice performer
  , disPerformer :: Maybe DisPerformer -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , ds9Performer :: Maybe Ds9Performer -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , gender :: Maybe Gender -- ^ Gender
  , dateOfBirth :: Maybe DateOfBirth -- ^ Date the performer was born
  , moviesPerformances :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  , placeOfDeath :: Maybe PlaceOfDeath -- ^ Place the performer died
  , tosPerformer :: Maybe TosPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , episodesPerformances :: Maybe [EpisodeBase] -- ^ Base episode, returned in search results
  , videoGamePerformer :: Maybe VideoGamePerformer -- ^ Whether it's a video game performer
  , filmPerformer :: Maybe FilmPerformer -- ^ Whether it's a performer that appeared in a Star Trek movie
  , placeOfBirth :: Maybe PlaceOfBirth -- ^ Place the performer was born
  , voyPerformer :: Maybe VoyPerformer -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  }
  deriving (Eq, Show)

performerFullSchema :: FC.Fleece schema => schema PerformerFull
performerFullSchema =
  FC.object $
    FC.constructor PerformerFull
      #+ FC.optional "birthName" birthName birthNameSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "moviesStandInPerformances" moviesStandInPerformances (FC.list movieBaseSchema)
      #+ FC.optional "tngPerformer" tngPerformer tngPerformerSchema
      #+ FC.optional "animalPerformer" animalPerformer animalPerformerSchema
      #+ FC.optional "standInPerformer" standInPerformer standInPerformerSchema
      #+ FC.optional "episodesStandInPerformances" episodesStandInPerformances (FC.list episodeBaseSchema)
      #+ FC.optional "tasPerformer" tasPerformer tasPerformerSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "dateOfDeath" dateOfDeath dateOfDeathSchema
      #+ FC.optional "stuntPerformer" stuntPerformer stuntPerformerSchema
      #+ FC.optional "moviesStuntPerformances" moviesStuntPerformances (FC.list movieBaseSchema)
      #+ FC.optional "entPerformer" entPerformer entPerformerSchema
      #+ FC.optional "episodesStuntPerformances" episodesStuntPerformances (FC.list episodeBaseSchema)
      #+ FC.optional "voicePerformer" voicePerformer voicePerformerSchema
      #+ FC.optional "disPerformer" disPerformer disPerformerSchema
      #+ FC.optional "ds9Performer" ds9Performer ds9PerformerSchema
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth dateOfBirthSchema
      #+ FC.optional "moviesPerformances" moviesPerformances (FC.list movieBaseSchema)
      #+ FC.optional "placeOfDeath" placeOfDeath placeOfDeathSchema
      #+ FC.optional "tosPerformer" tosPerformer tosPerformerSchema
      #+ FC.optional "episodesPerformances" episodesPerformances (FC.list episodeBaseSchema)
      #+ FC.optional "videoGamePerformer" videoGamePerformer videoGamePerformerSchema
      #+ FC.optional "filmPerformer" filmPerformer filmPerformerSchema
      #+ FC.optional "placeOfBirth" placeOfBirth placeOfBirthSchema
      #+ FC.optional "voyPerformer" voyPerformer voyPerformerSchema