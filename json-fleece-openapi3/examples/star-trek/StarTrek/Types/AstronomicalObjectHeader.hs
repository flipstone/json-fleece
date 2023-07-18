{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AstronomicalObjectHeader
  ( AstronomicalObjectHeader(..)
  , astronomicalObjectHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.AstronomicalObjectHeader.Name as Name
import qualified StarTrek.Types.AstronomicalObjectHeader.Uid as Uid

data AstronomicalObjectHeader = AstronomicalObjectHeader
  { uid :: Uid.Uid -- ^ Astronomical object's unique ID
  , name :: Name.Name -- ^ Astronomical object name
  }
  deriving (Eq, Show)

astronomicalObjectHeaderSchema :: FC.Fleece schema => schema AstronomicalObjectHeader
astronomicalObjectHeaderSchema =
  FC.object $
    FC.constructor AstronomicalObjectHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema