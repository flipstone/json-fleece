{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFull
  ( PerformerFull(..)
  , performerFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CharacterBase as CharacterBase
import qualified StarTrek.EpisodeBase as EpisodeBase
import qualified StarTrek.Gender as Gender
import qualified StarTrek.MovieBase as MovieBase
import qualified StarTrek.PerformerFull.AnimalPerformer as AnimalPerformer
import qualified StarTrek.PerformerFull.BirthName as BirthName
import qualified StarTrek.PerformerFull.DateOfBirth as DateOfBirth
import qualified StarTrek.PerformerFull.DateOfDeath as DateOfDeath
import qualified StarTrek.PerformerFull.DisPerformer as DisPerformer
import qualified StarTrek.PerformerFull.Ds9Performer as Ds9Performer
import qualified StarTrek.PerformerFull.EntPerformer as EntPerformer
import qualified StarTrek.PerformerFull.FilmPerformer as FilmPerformer
import qualified StarTrek.PerformerFull.Name as Name
import qualified StarTrek.PerformerFull.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.PerformerFull.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.PerformerFull.StandInPerformer as StandInPerformer
import qualified StarTrek.PerformerFull.StuntPerformer as StuntPerformer
import qualified StarTrek.PerformerFull.TasPerformer as TasPerformer
import qualified StarTrek.PerformerFull.TngPerformer as TngPerformer
import qualified StarTrek.PerformerFull.TosPerformer as TosPerformer
import qualified StarTrek.PerformerFull.Uid as Uid
import qualified StarTrek.PerformerFull.VideoGamePerformer as VideoGamePerformer
import qualified StarTrek.PerformerFull.VoicePerformer as VoicePerformer
import qualified StarTrek.PerformerFull.VoyPerformer as VoyPerformer

data PerformerFull = PerformerFull
  { birthName :: Maybe BirthName.BirthName -- ^ Performer birth name
  , name :: Name.Name -- ^ Performer name
  , moviesStandInPerformances :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , tngPerformer :: Maybe TngPerformer.TngPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , animalPerformer :: Maybe AnimalPerformer.AnimalPerformer -- ^ Whether it's an animal performer
  , standInPerformer :: Maybe StandInPerformer.StandInPerformer -- ^ Whether it's a stand-in performer
  , episodesStandInPerformances :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , tasPerformer :: Maybe TasPerformer.TasPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , uid :: Uid.Uid -- ^ Performer unique ID
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , dateOfDeath :: Maybe DateOfDeath.DateOfDeath -- ^ Date the performer died
  , stuntPerformer :: Maybe StuntPerformer.StuntPerformer -- ^ Whether it's a stunt performer
  , moviesStuntPerformances :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , entPerformer :: Maybe EntPerformer.EntPerformer -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , episodesStuntPerformances :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , voicePerformer :: Maybe VoicePerformer.VoicePerformer -- ^ Whether it's a voice performer
  , disPerformer :: Maybe DisPerformer.DisPerformer -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , ds9Performer :: Maybe Ds9Performer.Ds9Performer -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , gender :: Maybe Gender.Gender -- ^ Gender
  , dateOfBirth :: Maybe DateOfBirth.DateOfBirth -- ^ Date the performer was born
  , moviesPerformances :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place the performer died
  , tosPerformer :: Maybe TosPerformer.TosPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , episodesPerformances :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , videoGamePerformer :: Maybe VideoGamePerformer.VideoGamePerformer -- ^ Whether it's a video game performer
  , filmPerformer :: Maybe FilmPerformer.FilmPerformer -- ^ Whether it's a performer that appeared in a Star Trek movie
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place the performer was born
  , voyPerformer :: Maybe VoyPerformer.VoyPerformer -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  }
  deriving (Eq, Show)

performerFullSchema :: FC.Fleece schema => schema PerformerFull
performerFullSchema =
  FC.object $
    FC.constructor PerformerFull
      #+ FC.optional "birthName" birthName BirthName.birthNameSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "moviesStandInPerformances" moviesStandInPerformances (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "tngPerformer" tngPerformer TngPerformer.tngPerformerSchema
      #+ FC.optional "animalPerformer" animalPerformer AnimalPerformer.animalPerformerSchema
      #+ FC.optional "standInPerformer" standInPerformer StandInPerformer.standInPerformerSchema
      #+ FC.optional "episodesStandInPerformances" episodesStandInPerformances (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "tasPerformer" tasPerformer TasPerformer.tasPerformerSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "dateOfDeath" dateOfDeath DateOfDeath.dateOfDeathSchema
      #+ FC.optional "stuntPerformer" stuntPerformer StuntPerformer.stuntPerformerSchema
      #+ FC.optional "moviesStuntPerformances" moviesStuntPerformances (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "entPerformer" entPerformer EntPerformer.entPerformerSchema
      #+ FC.optional "episodesStuntPerformances" episodesStuntPerformances (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "voicePerformer" voicePerformer VoicePerformer.voicePerformerSchema
      #+ FC.optional "disPerformer" disPerformer DisPerformer.disPerformerSchema
      #+ FC.optional "ds9Performer" ds9Performer Ds9Performer.ds9PerformerSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth DateOfBirth.dateOfBirthSchema
      #+ FC.optional "moviesPerformances" moviesPerformances (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "tosPerformer" tosPerformer TosPerformer.tosPerformerSchema
      #+ FC.optional "episodesPerformances" episodesPerformances (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "videoGamePerformer" videoGamePerformer VideoGamePerformer.videoGamePerformerSchema
      #+ FC.optional "filmPerformer" filmPerformer FilmPerformer.filmPerformerSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "voyPerformer" voyPerformer VoyPerformer.voyPerformerSchema