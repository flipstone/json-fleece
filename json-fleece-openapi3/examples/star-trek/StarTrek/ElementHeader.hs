{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementHeader
  ( ElementHeader(..)
  , elementHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.ElementHeader.Name (Name, nameSchema)
import StarTrek.ElementHeader.Uid (Uid, uidSchema)

data ElementHeader = ElementHeader
  { name :: Name -- ^ Element name
  , uid :: Uid -- ^ Element unique ID
  }
  deriving (Eq, Show)

elementHeaderSchema :: FC.Fleece schema => schema ElementHeader
elementHeaderSchema =
  FC.object $
    FC.constructor ElementHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema