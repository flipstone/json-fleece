{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase
  ( PerformerBase(..)
  , performerBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.Gender (Gender, genderSchema)
import StarTrek.PerformerBase.AnimalPerformer (AnimalPerformer, animalPerformerSchema)
import StarTrek.PerformerBase.BirthName (BirthName, birthNameSchema)
import StarTrek.PerformerBase.DateOfBirth (DateOfBirth, dateOfBirthSchema)
import StarTrek.PerformerBase.DateOfDeath (DateOfDeath, dateOfDeathSchema)
import StarTrek.PerformerBase.DisPerformer (DisPerformer, disPerformerSchema)
import StarTrek.PerformerBase.Ds9Performer (Ds9Performer, ds9PerformerSchema)
import StarTrek.PerformerBase.EntPerformer (EntPerformer, entPerformerSchema)
import StarTrek.PerformerBase.FilmPerformer (FilmPerformer, filmPerformerSchema)
import StarTrek.PerformerBase.Name (Name, nameSchema)
import StarTrek.PerformerBase.PlaceOfBirth (PlaceOfBirth, placeOfBirthSchema)
import StarTrek.PerformerBase.PlaceOfDeath (PlaceOfDeath, placeOfDeathSchema)
import StarTrek.PerformerBase.StandInPerformer (StandInPerformer, standInPerformerSchema)
import StarTrek.PerformerBase.StuntPerformer (StuntPerformer, stuntPerformerSchema)
import StarTrek.PerformerBase.TasPerformer (TasPerformer, tasPerformerSchema)
import StarTrek.PerformerBase.TngPerformer (TngPerformer, tngPerformerSchema)
import StarTrek.PerformerBase.TosPerformer (TosPerformer, tosPerformerSchema)
import StarTrek.PerformerBase.Uid (Uid, uidSchema)
import StarTrek.PerformerBase.VideoGamePerformer (VideoGamePerformer, videoGamePerformerSchema)
import StarTrek.PerformerBase.VoicePerformer (VoicePerformer, voicePerformerSchema)
import StarTrek.PerformerBase.VoyPerformer (VoyPerformer, voyPerformerSchema)

data PerformerBase = PerformerBase
  { birthName :: Maybe BirthName -- ^ Performer birth name
  , name :: Name -- ^ Performer name
  , tngPerformer :: Maybe TngPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , animalPerformer :: Maybe AnimalPerformer -- ^ Whether it's an animal performer
  , standInPerformer :: Maybe StandInPerformer -- ^ Whether it's a stand-in performer
  , tasPerformer :: Maybe TasPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , uid :: Uid -- ^ Performer unique ID
  , dateOfDeath :: Maybe DateOfDeath -- ^ Date the performer died
  , stuntPerformer :: Maybe StuntPerformer -- ^ Whether it's a stunt performer
  , entPerformer :: Maybe EntPerformer -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , voicePerformer :: Maybe VoicePerformer -- ^ Whether it's a voice performer
  , disPerformer :: Maybe DisPerformer -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , ds9Performer :: Maybe Ds9Performer -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , gender :: Maybe Gender -- ^ Gender
  , dateOfBirth :: Maybe DateOfBirth -- ^ Date the performer was born
  , placeOfDeath :: Maybe PlaceOfDeath -- ^ Place the performer died
  , tosPerformer :: Maybe TosPerformer -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , videoGamePerformer :: Maybe VideoGamePerformer -- ^ Whether it's a video game performer
  , filmPerformer :: Maybe FilmPerformer -- ^ Whether it's a performer that appeared in a Star Trek movie
  , placeOfBirth :: Maybe PlaceOfBirth -- ^ Place the performer was born
  , voyPerformer :: Maybe VoyPerformer -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  }
  deriving (Eq, Show)

performerBaseSchema :: FC.Fleece schema => schema PerformerBase
performerBaseSchema =
  FC.object $
    FC.constructor PerformerBase
      #+ FC.optional "birthName" birthName birthNameSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "tngPerformer" tngPerformer tngPerformerSchema
      #+ FC.optional "animalPerformer" animalPerformer animalPerformerSchema
      #+ FC.optional "standInPerformer" standInPerformer standInPerformerSchema
      #+ FC.optional "tasPerformer" tasPerformer tasPerformerSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "dateOfDeath" dateOfDeath dateOfDeathSchema
      #+ FC.optional "stuntPerformer" stuntPerformer stuntPerformerSchema
      #+ FC.optional "entPerformer" entPerformer entPerformerSchema
      #+ FC.optional "voicePerformer" voicePerformer voicePerformerSchema
      #+ FC.optional "disPerformer" disPerformer disPerformerSchema
      #+ FC.optional "ds9Performer" ds9Performer ds9PerformerSchema
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth dateOfBirthSchema
      #+ FC.optional "placeOfDeath" placeOfDeath placeOfDeathSchema
      #+ FC.optional "tosPerformer" tosPerformer tosPerformerSchema
      #+ FC.optional "videoGamePerformer" videoGamePerformer videoGamePerformerSchema
      #+ FC.optional "filmPerformer" filmPerformer filmPerformerSchema
      #+ FC.optional "placeOfBirth" placeOfBirth placeOfBirthSchema
      #+ FC.optional "voyPerformer" voyPerformer voyPerformerSchema