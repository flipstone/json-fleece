{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialHeader
  ( MaterialHeader(..)
  , materialHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.MaterialHeader.Name (Name, nameSchema)
import StarTrek.MaterialHeader.Uid (Uid, uidSchema)

data MaterialHeader = MaterialHeader
  { name :: Name -- ^ Material name
  , uid :: Uid -- ^ Material unique ID
  }
  deriving (Eq, Show)

materialHeaderSchema :: FC.Fleece schema => schema MaterialHeader
materialHeaderSchema =
  FC.object $
    FC.constructor MaterialHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema