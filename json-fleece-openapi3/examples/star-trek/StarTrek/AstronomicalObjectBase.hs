{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectBase
  ( AstronomicalObjectBase(..)
  , astronomicalObjectBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectBase.Name (Name, nameSchema)
import StarTrek.AstronomicalObjectBase.Uid (Uid, uidSchema)
import StarTrek.AstronomicalObjectHeader (AstronomicalObjectHeader, astronomicalObjectHeaderSchema)
import StarTrek.AstronomicalObjectType (AstronomicalObjectType, astronomicalObjectTypeSchema)

data AstronomicalObjectBase = AstronomicalObjectBase
  { astronomicalObjectType :: AstronomicalObjectType -- ^ Astronomical object type
  , name :: Name -- ^ Astronomical object name
  , uid :: Uid -- ^ Astronomical object's unique ID
  , location :: Maybe AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  }
  deriving (Eq, Show)

astronomicalObjectBaseSchema :: FC.Fleece schema => schema AstronomicalObjectBase
astronomicalObjectBaseSchema =
  FC.object $
    FC.constructor AstronomicalObjectBase
      #+ FC.required "astronomicalObjectType" astronomicalObjectType astronomicalObjectTypeSchema
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "location" location astronomicalObjectHeaderSchema