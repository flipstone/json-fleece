{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase
  ( PerformerBase(..)
  , performerBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.Gender as Gender
import qualified StarTrek.Types.PerformerBase.AnimalPerformer as AnimalPerformer
import qualified StarTrek.Types.PerformerBase.BirthName as BirthName
import qualified StarTrek.Types.PerformerBase.DateOfBirth as DateOfBirth
import qualified StarTrek.Types.PerformerBase.DateOfDeath as DateOfDeath
import qualified StarTrek.Types.PerformerBase.DisPerformer as DisPerformer
import qualified StarTrek.Types.PerformerBase.Ds9Performer as Ds9Performer
import qualified StarTrek.Types.PerformerBase.EntPerformer as EntPerformer
import qualified StarTrek.Types.PerformerBase.FilmPerformer as FilmPerformer
import qualified StarTrek.Types.PerformerBase.Name as Name
import qualified StarTrek.Types.PerformerBase.PlaceOfBirth as PlaceOfBirth
import qualified StarTrek.Types.PerformerBase.PlaceOfDeath as PlaceOfDeath
import qualified StarTrek.Types.PerformerBase.StandInPerformer as StandInPerformer
import qualified StarTrek.Types.PerformerBase.StuntPerformer as StuntPerformer
import qualified StarTrek.Types.PerformerBase.TasPerformer as TasPerformer
import qualified StarTrek.Types.PerformerBase.TngPerformer as TngPerformer
import qualified StarTrek.Types.PerformerBase.TosPerformer as TosPerformer
import qualified StarTrek.Types.PerformerBase.Uid as Uid
import qualified StarTrek.Types.PerformerBase.VideoGamePerformer as VideoGamePerformer
import qualified StarTrek.Types.PerformerBase.VoicePerformer as VoicePerformer
import qualified StarTrek.Types.PerformerBase.VoyPerformer as VoyPerformer

data PerformerBase = PerformerBase
  { tngPerformer :: Maybe TngPerformer.TngPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , standInPerformer :: Maybe StandInPerformer.StandInPerformer -- ^ Whether it's a stand-in performer
  , animalPerformer :: Maybe AnimalPerformer.AnimalPerformer -- ^ Whether it's an animal performer
  , entPerformer :: Maybe EntPerformer.EntPerformer -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , ds9Performer :: Maybe Ds9Performer.Ds9Performer -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , videoGamePerformer :: Maybe VideoGamePerformer.VideoGamePerformer -- ^ Whether it's a video game performer
  , gender :: Maybe Gender.Gender -- ^ Gender
  , filmPerformer :: Maybe FilmPerformer.FilmPerformer -- ^ Whether it's a performer that appeared in a Star Trek movie
  , voicePerformer :: Maybe VoicePerformer.VoicePerformer -- ^ Whether it's a voice performer
  , uid :: Uid.Uid -- ^ Performer unique ID
  , voyPerformer :: Maybe VoyPerformer.VoyPerformer -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  , stuntPerformer :: Maybe StuntPerformer.StuntPerformer -- ^ Whether it's a stunt performer
  , dateOfDeath :: Maybe DateOfDeath.DateOfDeath -- ^ Date the performer died
  , tosPerformer :: Maybe TosPerformer.TosPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , tasPerformer :: Maybe TasPerformer.TasPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , placeOfBirth :: Maybe PlaceOfBirth.PlaceOfBirth -- ^ Place the performer was born
  , name :: Name.Name -- ^ Performer name
  , birthName :: Maybe BirthName.BirthName -- ^ Performer birth name
  , dateOfBirth :: Maybe DateOfBirth.DateOfBirth -- ^ Date the performer was born
  , placeOfDeath :: Maybe PlaceOfDeath.PlaceOfDeath -- ^ Place the performer died
  , disPerformer :: Maybe DisPerformer.DisPerformer -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  }
  deriving (Eq, Show)

performerBaseSchema :: FC.Fleece schema => schema PerformerBase
performerBaseSchema =
  FC.object $
    FC.constructor PerformerBase
      #+ FC.optional "tngPerformer" tngPerformer TngPerformer.tngPerformerSchema
      #+ FC.optional "standInPerformer" standInPerformer StandInPerformer.standInPerformerSchema
      #+ FC.optional "animalPerformer" animalPerformer AnimalPerformer.animalPerformerSchema
      #+ FC.optional "entPerformer" entPerformer EntPerformer.entPerformerSchema
      #+ FC.optional "ds9Performer" ds9Performer Ds9Performer.ds9PerformerSchema
      #+ FC.optional "videoGamePerformer" videoGamePerformer VideoGamePerformer.videoGamePerformerSchema
      #+ FC.optional "gender" gender Gender.genderSchema
      #+ FC.optional "filmPerformer" filmPerformer FilmPerformer.filmPerformerSchema
      #+ FC.optional "voicePerformer" voicePerformer VoicePerformer.voicePerformerSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "voyPerformer" voyPerformer VoyPerformer.voyPerformerSchema
      #+ FC.optional "stuntPerformer" stuntPerformer StuntPerformer.stuntPerformerSchema
      #+ FC.optional "dateOfDeath" dateOfDeath DateOfDeath.dateOfDeathSchema
      #+ FC.optional "tosPerformer" tosPerformer TosPerformer.tosPerformerSchema
      #+ FC.optional "tasPerformer" tasPerformer TasPerformer.tasPerformerSchema
      #+ FC.optional "placeOfBirth" placeOfBirth PlaceOfBirth.placeOfBirthSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "birthName" birthName BirthName.birthNameSchema
      #+ FC.optional "dateOfBirth" dateOfBirth DateOfBirth.dateOfBirthSchema
      #+ FC.optional "placeOfDeath" placeOfDeath PlaceOfDeath.placeOfDeathSchema
      #+ FC.optional "disPerformer" disPerformer DisPerformer.disPerformerSchema