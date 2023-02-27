{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase
  ( PerformerBase(..)
  , performerBaseSchema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.Gender (Gender, genderSchema)

data PerformerBase = PerformerBase
  { birthName :: Maybe Text -- ^ Performer birth name
  , name :: Text -- ^ Performer name
  , tngPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: The Next Generation
  , animalPerformer :: Maybe Bool -- ^ Whether it's an animal performer
  , standInPerformer :: Maybe Bool -- ^ Whether it's a stand-in performer
  , tasPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: The Animated Series
  , uid :: Text -- ^ Performer unique ID
  , dateOfDeath :: Maybe Day -- ^ Date the performer died
  , stuntPerformer :: Maybe Bool -- ^ Whether it's a stunt performer
  , entPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , voicePerformer :: Maybe Bool -- ^ Whether it's a voice performer
  , disPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , ds9Performer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , gender :: Maybe Gender -- ^ Gender
  , dateOfBirth :: Maybe Day -- ^ Date the performer was born
  , placeOfDeath :: Maybe Text -- ^ Place the performer died
  , tosPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: The Original Series
  , videoGamePerformer :: Maybe Bool -- ^ Whether it's a video game performer
  , filmPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in a Star Trek movie
  , placeOfBirth :: Maybe Text -- ^ Place the performer was born
  , voyPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Voyager
  }
  deriving (Eq, Show)

performerBaseSchema :: FC.Fleece schema => schema PerformerBase
performerBaseSchema =
  FC.object $
    FC.constructor PerformerBase
      #+ FC.optional "birthName" birthName FC.text
      #+ FC.required "name" name FC.text
      #+ FC.optional "tngPerformer" tngPerformer FC.boolean
      #+ FC.optional "animalPerformer" animalPerformer FC.boolean
      #+ FC.optional "standInPerformer" standInPerformer FC.boolean
      #+ FC.optional "tasPerformer" tasPerformer FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "dateOfDeath" dateOfDeath FC.day
      #+ FC.optional "stuntPerformer" stuntPerformer FC.boolean
      #+ FC.optional "entPerformer" entPerformer FC.boolean
      #+ FC.optional "voicePerformer" voicePerformer FC.boolean
      #+ FC.optional "disPerformer" disPerformer FC.boolean
      #+ FC.optional "ds9Performer" ds9Performer FC.boolean
      #+ FC.optional "gender" gender genderSchema
      #+ FC.optional "dateOfBirth" dateOfBirth FC.day
      #+ FC.optional "placeOfDeath" placeOfDeath FC.text
      #+ FC.optional "tosPerformer" tosPerformer FC.boolean
      #+ FC.optional "videoGamePerformer" videoGamePerformer FC.boolean
      #+ FC.optional "filmPerformer" filmPerformer FC.boolean
      #+ FC.optional "placeOfBirth" placeOfBirth FC.text
      #+ FC.optional "voyPerformer" voyPerformer FC.boolean