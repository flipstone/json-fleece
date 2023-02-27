{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationHeader
  ( LocationHeader(..)
  , locationHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.LocationHeader.Name (Name, nameSchema)
import StarTrek.LocationHeader.Uid (Uid, uidSchema)

data LocationHeader = LocationHeader
  { name :: Name -- ^ Location name
  , uid :: Uid -- ^ Location unique ID
  }
  deriving (Eq, Show)

locationHeaderSchema :: FC.Fleece schema => schema LocationHeader
locationHeaderSchema =
  FC.object $
    FC.constructor LocationHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema