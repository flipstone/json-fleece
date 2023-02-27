{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerHeader
  ( PerformerHeader(..)
  , performerHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.PerformerHeader.Name (Name, nameSchema)
import StarTrek.PerformerHeader.Uid (Uid, uidSchema)

data PerformerHeader = PerformerHeader
  { name :: Name -- ^ Performer name
  , uid :: Uid -- ^ Performer unique ID
  }
  deriving (Eq, Show)

performerHeaderSchema :: FC.Fleece schema => schema PerformerHeader
performerHeaderSchema =
  FC.object $
    FC.constructor PerformerHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema