{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectBase
  ( AstronomicalObjectBase(..)
  , astronomicalObjectBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.AstronomicalObjectBase.Name as Name
import qualified StarTrek.AstronomicalObjectBase.Uid as Uid
import qualified StarTrek.AstronomicalObjectHeader as AstronomicalObjectHeader
import qualified StarTrek.AstronomicalObjectType as AstronomicalObjectType

data AstronomicalObjectBase = AstronomicalObjectBase
  { astronomicalObjectType :: AstronomicalObjectType.AstronomicalObjectType -- ^ Astronomical object type
  , name :: Name.Name -- ^ Astronomical object name
  , uid :: Uid.Uid -- ^ Astronomical object's unique ID
  , location :: Maybe AstronomicalObjectHeader.AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  }
  deriving (Eq, Show)

astronomicalObjectBaseSchema :: FC.Fleece schema => schema AstronomicalObjectBase
astronomicalObjectBaseSchema =
  FC.object $
    FC.constructor AstronomicalObjectBase
      #+ FC.required "astronomicalObjectType" astronomicalObjectType AstronomicalObjectType.astronomicalObjectTypeSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "location" location AstronomicalObjectHeader.astronomicalObjectHeaderSchema