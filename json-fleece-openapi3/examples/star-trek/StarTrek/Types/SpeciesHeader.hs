{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesHeader
  ( SpeciesHeader(..)
  , speciesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.SpeciesHeader.Name as Name
import qualified StarTrek.Types.SpeciesHeader.Uid as Uid

data SpeciesHeader = SpeciesHeader
  { uid :: Uid.Uid -- ^ Species unique ID
  , name :: Name.Name -- ^ Species name
  }
  deriving (Eq, Show)

speciesHeaderSchema :: FC.Fleece schema => schema SpeciesHeader
speciesHeaderSchema =
  FC.object $
    FC.constructor SpeciesHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema