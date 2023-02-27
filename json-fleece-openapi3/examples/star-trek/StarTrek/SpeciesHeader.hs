{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesHeader
  ( SpeciesHeader(..)
  , speciesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.SpeciesHeader.Name (Name, nameSchema)
import StarTrek.SpeciesHeader.Uid (Uid, uidSchema)

data SpeciesHeader = SpeciesHeader
  { name :: Name -- ^ Species name
  , uid :: Uid -- ^ Species unique ID
  }
  deriving (Eq, Show)

speciesHeaderSchema :: FC.Fleece schema => schema SpeciesHeader
speciesHeaderSchema =
  FC.object $
    FC.constructor SpeciesHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema