{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AstronomicalObjectHeader
  ( AstronomicalObjectHeader(..)
  , astronomicalObjectHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.AstronomicalObjectHeader.Name (Name, nameSchema)
import StarTrek.AstronomicalObjectHeader.Uid (Uid, uidSchema)

data AstronomicalObjectHeader = AstronomicalObjectHeader
  { name :: Name -- ^ Astronomical object name
  , uid :: Uid -- ^ Astronomical object's unique ID
  }
  deriving (Eq, Show)

astronomicalObjectHeaderSchema :: FC.Fleece schema => schema AstronomicalObjectHeader
astronomicalObjectHeaderSchema =
  FC.object $
    FC.constructor AstronomicalObjectHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema