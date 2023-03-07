{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AstronomicalObjectFull
  ( AstronomicalObjectFull(..)
  , astronomicalObjectFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AstronomicalObjectBase as AstronomicalObjectBase
import qualified StarTrek.Types.AstronomicalObjectFull.Name as Name
import qualified StarTrek.Types.AstronomicalObjectFull.Uid as Uid
import qualified StarTrek.Types.AstronomicalObjectType as AstronomicalObjectType

data AstronomicalObjectFull = AstronomicalObjectFull
  { astronomicalObjectType :: AstronomicalObjectType.AstronomicalObjectType -- ^ Astronomical object type
  , name :: Name.Name -- ^ Astronomical object name
  , uid :: Uid.Uid -- ^ Astronomical object's unique ID
  , location :: Maybe AstronomicalObjectBase.AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , astronomicalObjects :: Maybe [AstronomicalObjectBase.AstronomicalObjectBase] -- ^ Base astronomical object, returned in search results
  }
  deriving (Eq, Show)

astronomicalObjectFullSchema :: FC.Fleece schema => schema AstronomicalObjectFull
astronomicalObjectFullSchema =
  FC.object $
    FC.constructor AstronomicalObjectFull
      #+ FC.required "astronomicalObjectType" astronomicalObjectType AstronomicalObjectType.astronomicalObjectTypeSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "location" location AstronomicalObjectBase.astronomicalObjectBaseSchema
      #+ FC.optional "astronomicalObjects" astronomicalObjects (FC.list AstronomicalObjectBase.astronomicalObjectBaseSchema)