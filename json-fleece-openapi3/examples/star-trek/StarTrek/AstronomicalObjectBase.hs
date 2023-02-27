{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectBase
  ( AstronomicalObjectBase(..)
  , astronomicalObjectBaseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectHeader (AstronomicalObjectHeader, astronomicalObjectHeaderSchema)
import StarTrek.AstronomicalObjectType (AstronomicalObjectType, astronomicalObjectTypeSchema)

data AstronomicalObjectBase = AstronomicalObjectBase
  { astronomicalObjectType :: AstronomicalObjectType -- ^ Astronomical object type
  , name :: Text -- ^ Astronomical object name
  , uid :: Text -- ^ Astronomical object's unique ID
  , location :: Maybe AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  }
  deriving (Eq, Show)

astronomicalObjectBaseSchema :: FC.Fleece schema => schema AstronomicalObjectBase
astronomicalObjectBaseSchema =
  FC.object $
    FC.constructor AstronomicalObjectBase
      #+ FC.required "astronomicalObjectType" astronomicalObjectType astronomicalObjectTypeSchema
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "location" location astronomicalObjectHeaderSchema