{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase
  ( PerformerBase(..)
  , performerBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
  , dateOfDeath :: Maybe Text -- ^ Date the performer died
  , stuntPerformer :: Maybe Bool -- ^ Whether it's a stunt performer
  , entPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Enterprise
  , voicePerformer :: Maybe Bool -- ^ Whether it's a voice performer
  , disPerformer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Discovery
  , ds9Performer :: Maybe Bool -- ^ Whether it's a performer that appeared in Star Trek: Deep Space Nine
  , gender :: Maybe Gender -- ^ Gender
  , dateOfBirth :: Maybe Text -- ^ Date the performer was born
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "birthName" birthName FC.text
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "tngPerformer" tngPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "animalPerformer" animalPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "standInPerformer" standInPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "tasPerformer" tasPerformer FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "dateOfDeath" dateOfDeath FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "stuntPerformer" stuntPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "entPerformer" entPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "voicePerformer" voicePerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "disPerformer" disPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "ds9Performer" ds9Performer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "gender" gender genderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "dateOfBirth" dateOfBirth FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "placeOfDeath" placeOfDeath FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "tosPerformer" tosPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "videoGamePerformer" videoGamePerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "filmPerformer" filmPerformer FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "placeOfBirth" placeOfBirth FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "voyPerformer" voyPerformer FC.boolean