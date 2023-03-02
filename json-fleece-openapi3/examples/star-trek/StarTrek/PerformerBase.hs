{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase
  ( PerformerBase(..)
  , performerBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Gender as Gender
import qualified StarTrek.PerformerBase.AnimalPerformer as AnimalPerformer
import qualified StarTrek.PerformerBase.BirthName as BirthName
import qualified StarTrek.PerformerBase.DateOfBirth as DateOfBirth
import qualified StarTrek.PerformerBase.DateOfDeath as DateOfDeath
import qualified StarTrek.PerformerBase.DisPerformer as DisPerformer
import qualified StarTrek.PerformerBase.Ds9Performer as Ds9Performer
import qualified StarTrek.PerformerBase.EntPerformer as EntPerformer
import qualified StarTrek.PerformerBase.FilmPerformer as FilmPerformer
import qualified StarTrek.PerformerBase.Name as Name
import qualified StarTrek.PerformerBase.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.PerformerBase.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.PerformerBase.StandInPerformer as StandInPerformer
import qualified StarTrek.PerformerBase.StuntPerformer as StuntPerformer
import qualified StarTrek.PerformerBase.TasPerformer as TasPerformer
import qualified StarTrek.PerformerBase.TngPerformer as TngPerformer
import qualified StarTrek.PerformerBase.TosPerformer as TosPerformer
import qualified StarTrek.PerformerBase.Uid as Uid
import qualified StarTrek.PerformerBase.VideoGamePerformer as VideoGamePerformer
import qualified StarTrek.PerformerBase.VoicePerformer as VoicePerformer
import qualified StarTrek.PerformerBase.VoyPerformer as VoyPerformer

data PerformerBase = PerformerBase
  { birthName :: Maybe BirthName.BirthName -- ^ Performer birth name
  , name :: Name.Name -- ^ Performer name
  , tngPerformer :: Maybe TngPerformer.TngPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , animalPerformer :: Maybe AnimalPerformer.AnimalPerformer -- ^ Whether it's an animal performer
  , standInPerformer :: Maybe StandInPerformer.StandInPerformer -- ^ Whether it's a stand-in performer
  , tasPerformer :: Maybe TasPerformer.TasPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , uid :: Uid.Uid -- ^ Performer unique ID
  , dateOfDeath :: Maybe DateOfDeath.DateOfDeath -- ^ Date the performer died
  , stuntPerformer :: Maybe StuntPerformer.StuntPerformer -- ^ Whether it's a stunt performer
  , entPerformer :: Maybe EntPerformer.EntPerformer -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , voicePerformer :: Maybe VoicePerformer.VoicePerformer -- ^ Whether it's a voice performer
  , disPerformer :: Maybe DisPerformer.DisPerformer -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , ds9Performer :: Maybe Ds9Performer.Ds9Performer -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , gender :: Maybe Gender.Gender -- ^ Gender
  , dateOfBirth :: Maybe DateOfBirth.DateOfBirth -- ^ Date the performer was born
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place the performer died
  , tosPerformer :: Maybe TosPerformer.TosPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , videoGamePerformer :: Maybe VideoGamePerformer.VideoGamePerformer -- ^ Whether it's a video game performer
  , filmPerformer :: Maybe FilmPerformer.FilmPerformer -- ^ Whether it's a performer that appeared in a Star Trek movie
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place the performer was born
  , voyPerformer :: Maybe VoyPerformer.VoyPerformer -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  }
  deriving (Eq, Show)

performerBaseSchema :: FC.Fleece schema => schema PerformerBase
performerBaseSchema =
  FC.object $
    FC.constructor PerformerBase
      #+ FC.optional "birthName" birthName BirthName.birthNameSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "tngPerformer" tngPerformer TngPerformer.tngPerformerSchema
      #+ FC.optional "animalPerformer" animalPerformer AnimalPerformer.animalPerformerSchema
      #+ FC.optional "standInPerformer" standInPerformer StandInPerformer.standInPerformerSchema
      #+ FC.optional "tasPerformer" tasPerformer TasPerformer.tasPerformerSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "dateOfDeath" dateOfDeath DateOfDeath.dateOfDeathSchema
      #+ FC.optional "stuntPerformer" stuntPerformer StuntPerformer.stuntPerformerSchema
      #+ FC.optional "entPerformer" entPerformer EntPerformer.entPerformerSchema
      #+ FC.optional "voicePerformer" voicePerformer VoicePerformer.voicePerformerSchema
      #+ FC.optional "disPerformer" disPerformer DisPerformer.disPerformerSchema
      #+ FC.optional "ds9Performer" ds9Performer Ds9Performer.ds9PerformerSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth DateOfBirth.dateOfBirthSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "tosPerformer" tosPerformer TosPerformer.tosPerformerSchema
      #+ FC.optional "videoGamePerformer" videoGamePerformer VideoGamePerformer.videoGamePerformerSchema
      #+ FC.optional "filmPerformer" filmPerformer FilmPerformer.filmPerformerSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.optional "voyPerformer" voyPerformer VoyPerformer.voyPerformerSchema