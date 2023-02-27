{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictHeader
  ( ConflictHeader(..)
  , conflictHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.ConflictHeader.Name (Name, nameSchema)
import StarTrek.ConflictHeader.Uid (Uid, uidSchema)

data ConflictHeader = ConflictHeader
  { name :: Name -- ^ Conflict name
  , uid :: Uid -- ^ Conflict unique ID
  }
  deriving (Eq, Show)

conflictHeaderSchema :: FC.Fleece schema => schema ConflictHeader
conflictHeaderSchema =
  FC.object $
    FC.constructor ConflictHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema