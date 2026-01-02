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
  { name :: Name.Name -- ^ Occupation name
  , uid :: Uid.Uid -- ^ Occupation unique ID
  }
  deriving (Eq, Show)

occupationHeaderSchema :: FC.Fleece t => FC.Schema t OccupationHeader
occupationHeaderSchema =
  FC.object $
    FC.constructor OccupationHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema