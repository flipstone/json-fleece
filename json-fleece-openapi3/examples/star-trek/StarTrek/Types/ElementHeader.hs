{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementHeader
  ( ElementHeader(..)
  , elementHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.ElementHeader.Name as Name
import qualified StarTrek.Types.ElementHeader.Uid as Uid

data ElementHeader = ElementHeader
  { uid :: Uid.Uid -- ^ Element unique ID
  , name :: Name.Name -- ^ Element name
  }
  deriving (Eq, Show)

elementHeaderSchema :: FC.Fleece schema => schema ElementHeader
elementHeaderSchema =
  FC.object $
    FC.constructor ElementHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema