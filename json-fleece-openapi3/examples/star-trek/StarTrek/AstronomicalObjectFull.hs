{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectFull
  ( AstronomicalObjectFull(..)
  , astronomicalObjectFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectBase (AstronomicalObjectBase, astronomicalObjectBaseSchema)
import StarTrek.AstronomicalObjectFull.Name (Name, nameSchema)
import StarTrek.AstronomicalObjectFull.Uid (Uid, uidSchema)
import StarTrek.AstronomicalObjectType (AstronomicalObjectType, astronomicalObjectTypeSchema)

data AstronomicalObjectFull = AstronomicalObjectFull
  { astronomicalObjectType :: AstronomicalObjectType -- ^ Astronomical object type
  , name :: Name -- ^ Astronomical object name
  , uid :: Uid -- ^ Astronomical object's unique ID
  , location :: Maybe AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , astronomicalObjects :: Maybe [AstronomicalObjectBase] -- ^ Base astronomical object, returned in search results
  }
  deriving (Eq, Show)

astronomicalObjectFullSchema :: FC.Fleece schema => schema AstronomicalObjectFull
astronomicalObjectFullSchema =
  FC.object $
    FC.constructor AstronomicalObjectFull
      #+ FC.required "astronomicalObjectType" astronomicalObjectType astronomicalObjectTypeSchema
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "location" location astronomicalObjectBaseSchema
      #+ FC.optional "astronomicalObjects" astronomicalObjects (FC.list astronomicalObjectBaseSchema)