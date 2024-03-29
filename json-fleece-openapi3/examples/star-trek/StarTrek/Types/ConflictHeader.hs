{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictHeader
  ( ConflictHeader(..)
  , conflictHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.ConflictHeader.Name as Name
import qualified StarTrek.Types.ConflictHeader.Uid as Uid

data ConflictHeader = ConflictHeader
  { uid :: Uid.Uid -- ^ Conflict unique ID
  , name :: Name.Name -- ^ Conflict name
  }
  deriving (Eq, Show)

conflictHeaderSchema :: FC.Fleece schema => schema ConflictHeader
conflictHeaderSchema =
  FC.object $
    FC.constructor ConflictHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema