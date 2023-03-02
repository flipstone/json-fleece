{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectHeader
  ( AstronomicalObjectHeader(..)
  , astronomicalObjectHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.AstronomicalObjectHeader.Name as Name
import qualified StarTrek.AstronomicalObjectHeader.Uid as Uid

data AstronomicalObjectHeader = AstronomicalObjectHeader
  { name :: Name.Name -- ^ Astronomical object name
  , uid :: Uid.Uid -- ^ Astronomical object's unique ID
  }
  deriving (Eq, Show)

astronomicalObjectHeaderSchema :: FC.Fleece schema => schema AstronomicalObjectHeader
astronomicalObjectHeaderSchema =
  FC.object $
    FC.constructor AstronomicalObjectHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema