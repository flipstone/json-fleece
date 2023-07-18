{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OccupationHeader
  ( OccupationHeader(..)
  , occupationHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.OccupationHeader.Name as Name
import qualified StarTrek.Types.OccupationHeader.Uid as Uid

data OccupationHeader = OccupationHeader
  { uid :: Uid.Uid -- ^ Occupation unique ID
  , name :: Name.Name -- ^ Occupation name
  }
  deriving (Eq, Show)

occupationHeaderSchema :: FC.Fleece schema => schema OccupationHeader
occupationHeaderSchema =
  FC.object $
    FC.constructor OccupationHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema