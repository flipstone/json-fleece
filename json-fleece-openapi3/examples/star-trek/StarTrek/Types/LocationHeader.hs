{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationHeader
  ( LocationHeader(..)
  , locationHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.LocationHeader.Name as Name
import qualified StarTrek.Types.LocationHeader.Uid as Uid

data LocationHeader = LocationHeader
  { uid :: Uid.Uid -- ^ Location unique ID
  , name :: Name.Name -- ^ Location name
  }
  deriving (Eq, Show)

locationHeaderSchema :: FC.Fleece schema => schema LocationHeader
locationHeaderSchema =
  FC.object $
    FC.constructor LocationHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema