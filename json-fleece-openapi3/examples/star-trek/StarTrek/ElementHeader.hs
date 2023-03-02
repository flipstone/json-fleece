{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ElementHeader
  ( ElementHeader(..)
  , elementHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.ElementHeader.Name as Name
import qualified StarTrek.ElementHeader.Uid as Uid

data ElementHeader = ElementHeader
  { name :: Name.Name -- ^ Element name
  , uid :: Uid.Uid -- ^ Element unique ID
  }
  deriving (Eq, Show)

elementHeaderSchema :: FC.Fleece schema => schema ElementHeader
elementHeaderSchema =
  FC.object $
    FC.constructor ElementHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema