{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationHeader
  ( OccupationHeader(..)
  , occupationHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.OccupationHeader.Name (Name, nameSchema)
import StarTrek.OccupationHeader.Uid (Uid, uidSchema)

data OccupationHeader = OccupationHeader
  { name :: Name -- ^ Occupation name
  , uid :: Uid -- ^ Occupation unique ID
  }
  deriving (Eq, Show)

occupationHeaderSchema :: FC.Fleece schema => schema OccupationHeader
occupationHeaderSchema =
  FC.object $
    FC.constructor OccupationHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema