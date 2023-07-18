{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull
  ( PerformerFull(..)
  , performerFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.EpisodeBase as EpisodeBase
import qualified StarTrek.Types.Gender as Gender
import qualified StarTrek.Types.MovieBase as MovieBase
import qualified StarTrek.Types.PerformerFull.AnimalPerformer as AnimalPerformer
import qualified StarTrek.Types.PerformerFull.BirthName as BirthName
import qualified StarTrek.Types.PerformerFull.DateOfBirth as DateOfBirth
import qualified StarTrek.Types.PerformerFull.DateOfDeath as DateOfDeath
import qualified StarTrek.Types.PerformerFull.DisPerformer as DisPerformer
import qualified StarTrek.Types.PerformerFull.Ds9Performer as Ds9Performer
import qualified StarTrek.Types.PerformerFull.EntPerformer as EntPerformer
import qualified StarTrek.Types.PerformerFull.FilmPerformer as FilmPerformer
import qualified StarTrek.Types.PerformerFull.Name as Name
import qualified StarTrek.Types.PerformerFull.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.Types.PerformerFull.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.Types.PerformerFull.StandInPerformer as StandInPerformer
import qualified StarTrek.Types.PerformerFull.StuntPerformer as StuntPerformer
import qualified StarTrek.Types.PerformerFull.TasPerformer as TasPerformer
import qualified StarTrek.Types.PerformerFull.TngPerformer as TngPerformer
import qualified StarTrek.Types.PerformerFull.TosPerformer as TosPerformer
import qualified StarTrek.Types.PerformerFull.Uid as Uid
import qualified StarTrek.Types.PerformerFull.VideoGamePerformer as VideoGamePerformer
import qualified StarTrek.Types.PerformerFull.VoicePerformer as VoicePerformer
import qualified StarTrek.Types.PerformerFull.VoyPerformer as VoyPerformer

data PerformerFull = PerformerFull
  { tasPerformer :: Maybe TasPerformer.TasPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , birthName :: Maybe BirthName.BirthName -- ^ Performer birth name
  , ds9Performer :: Maybe Ds9Performer.Ds9Performer -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , tngPerformer :: Maybe TngPerformer.TngPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , dateOfBirth :: Maybe DateOfBirth.DateOfBirth -- ^ Date the performer was born
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place the performer was born
  , moviesStuntPerformances :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , episodesStandInPerformances :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , uid :: Uid.Uid -- ^ Performer unique ID
  , stuntPerformer :: Maybe StuntPerformer.StuntPerformer -- ^ Whether it's a stunt performer
  , episodesPerformances :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , standInPerformer :: Maybe StandInPerformer.StandInPerformer -- ^ Whether it's a stand-in performer
  , videoGamePerformer :: Maybe VideoGamePerformer.VideoGamePerformer -- ^ Whether it's a video game performer
  , filmPerformer :: Maybe FilmPerformer.FilmPerformer -- ^ Whether it's a performer that appeared in a Star Trek movie
  , episodesStuntPerformances :: Maybe [EpisodeBase.EpisodeBase] -- ^ Base episode, returned in search results
  , gender :: Maybe Gender.Gender -- ^ Gender
  , moviesPerformances :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , disPerformer :: Maybe DisPerformer.DisPerformer -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , animalPerformer :: Maybe AnimalPerformer.AnimalPerformer -- ^ Whether it's an animal performer
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place the performer died
  , moviesStandInPerformances :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , dateOfDeath :: Maybe DateOfDeath.DateOfDeath -- ^ Date the performer died
  , tosPerformer :: Maybe TosPerformer.TosPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , entPerformer :: Maybe EntPerformer.EntPerformer -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , voicePerformer :: Maybe VoicePerformer.VoicePerformer -- ^ Whether it's a voice performer
  , name :: Name.Name -- ^ Performer name
  , voyPerformer :: Maybe VoyPerformer.VoyPerformer -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  }
  deriving (Eq, Show)

performerFullSchema :: FC.Fleece schema => schema PerformerFull
performerFullSchema =
  FC.object $
    FC.constructor PerformerFull
      #+ FC.optional "tasPerformer" tasPerformer TasPerformer.tasPerformerSchema
      #+ FC.optional "birthName" birthName BirthName.birthNameSchema
      #+ FC.optional "ds9Performer" ds9Performer Ds9Performer.ds9PerformerSchema
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "tngPerformer" tngPerformer TngPerformer.tngPerformerSchema
      #+ FC.optional "dateOfBirth" dateOfBirth DateOfBirth.dateOfBirthSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "moviesStuntPerformances" moviesStuntPerformances (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "episodesStandInPerformances" episodesStandInPerformances (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "stuntPerformer" stuntPerformer StuntPerformer.stuntPerformerSchema
      #+ FC.optional "episodesPerformances" episodesPerformances (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "standInPerformer" standInPerformer StandInPerformer.standInPerformerSchema
      #+ FC.optional "videoGamePerformer" videoGamePerformer VideoGamePerformer.videoGamePerformerSchema
      #+ FC.optional "filmPerformer" filmPerformer FilmPerformer.filmPerformerSchema
      #+ FC.optional "episodesStuntPerformances" episodesStuntPerformances (FC.list EpisodeBase.episodeBaseSchema)
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "moviesPerformances" moviesPerformances (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "disPerformer" disPerformer DisPerformer.disPerformerSchema
      #+ FC.optional "animalPerformer" animalPerformer AnimalPerformer.animalPerformerSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "moviesStandInPerformances" moviesStandInPerformances (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "dateOfDeath" dateOfDeath DateOfDeath.dateOfDeathSchema
      #+ FC.optional "tosPerformer" tosPerformer TosPerformer.tosPerformerSchema
      #+ FC.optional "entPerformer" entPerformer EntPerformer.entPerformerSchema
      #+ FC.optional "voicePerformer" voicePerformer VoicePerformer.voicePerformerSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "voyPerformer" voyPerformer VoyPerformer.voyPerformerSchema