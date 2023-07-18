{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AstronomicalObjectBase
  ( AstronomicalObjectBase(..)
  , astronomicalObjectBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AstronomicalObjectBase.Name as Name
import qualified StarTrek.Types.AstronomicalObjectBase.Uid as Uid
import qualified StarTrek.Types.AstronomicalObjectHeader as AstronomicalObjectHeader
import qualified StarTrek.Types.AstronomicalObjectType as AstronomicalObjectType

data AstronomicalObjectBase = AstronomicalObjectBase
  { astronomicalObjectType :: AstronomicalObjectType.AstronomicalObjectType -- ^ Astronomical object type
  , uid :: Uid.Uid -- ^ Astronomical object's unique ID
  , location :: Maybe AstronomicalObjectHeader.AstronomicalObjectHeader -- ^ Header astronomical object, embedded in other objects
  , name :: Name.Name -- ^ Astronomical object name
  }
  deriving (Eq, Show)

astronomicalObjectBaseSchema :: FC.Fleece schema => schema AstronomicalObjectBase
astronomicalObjectBaseSchema =
  FC.object $
    FC.constructor AstronomicalObjectBase
      #+ FC.required "astronomicalObjectType" astronomicalObjectType AstronomicalObjectType.astronomicalObjectTypeSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "location" location AstronomicalObjectHeader.astronomicalObjectHeaderSchema
      #+ FC.required "name" name Name.nameSchema