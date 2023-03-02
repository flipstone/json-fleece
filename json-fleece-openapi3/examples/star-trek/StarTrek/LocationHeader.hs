{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationHeader
  ( LocationHeader(..)
  , locationHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.LocationHeader.Name as Name
import qualified StarTrek.LocationHeader.Uid as Uid

data LocationHeader = LocationHeader
  { name :: Name.Name -- ^ Location name
  , uid :: Uid.Uid -- ^ Location unique ID
  }
  deriving (Eq, Show)

locationHeaderSchema :: FC.Fleece schema => schema LocationHeader
locationHeaderSchema =
  FC.object $
    FC.constructor LocationHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema