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
  { name :: Name.Name -- ^ Conflict name
  , uid :: Uid.Uid -- ^ Conflict unique ID
  }
  deriving (Eq, Show)

conflictHeaderSchema :: FC.Fleece t => FC.Schema t ConflictHeader
conflictHeaderSchema =
  FC.object $
    FC.constructor ConflictHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema