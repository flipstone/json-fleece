{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectFull
  ( AstronomicalObjectFull(..)
  , astronomicalObjectFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AstronomicalObjectBase (AstronomicalObjectBase, astronomicalObjectBaseSchema)
import StarTrek.AstronomicalObjectType (AstronomicalObjectType, astronomicalObjectTypeSchema)

data AstronomicalObjectFull = AstronomicalObjectFull
  { astronomicalObjectType :: AstronomicalObjectType -- ^ Astronomical object type
  , name :: Text -- ^ Astronomical object name
  , uid :: Text -- ^ Astronomical object's unique ID
  , location :: Maybe AstronomicalObjectBase -- ^ Base astronomical object, returned in search results
  , astronomicalObjects :: Maybe [AstronomicalObjectBase] -- ^ Astronomical objects located in this astronomical object, like planets in a star system
  }
  deriving (Eq, Show)

astronomicalObjectFullSchema :: FC.Fleece schema => schema AstronomicalObjectFull
astronomicalObjectFullSchema =
  FC.object $
    FC.constructor AstronomicalObjectFull
      #+ FC.required "astronomicalObjectType" astronomicalObjectType astronomicalObjectTypeSchema
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "location" location astronomicalObjectBaseSchema
      #+ FC.optional "astronomicalObjects" astronomicalObjects (FC.list astronomicalObjectBaseSchema)