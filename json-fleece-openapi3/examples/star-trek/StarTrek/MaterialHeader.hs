{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialHeader
  ( MaterialHeader(..)
  , materialHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.MaterialHeader.Name as Name
import qualified StarTrek.MaterialHeader.Uid as Uid

data MaterialHeader = MaterialHeader
  { name :: Name.Name -- ^ Material name
  , uid :: Uid.Uid -- ^ Material unique ID
  }
  deriving (Eq, Show)

materialHeaderSchema :: FC.Fleece schema => schema MaterialHeader
materialHeaderSchema =
  FC.object $
    FC.constructor MaterialHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema