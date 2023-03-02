{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerHeader
  ( PerformerHeader(..)
  , performerHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.PerformerHeader.Name as Name
import qualified StarTrek.PerformerHeader.Uid as Uid

data PerformerHeader = PerformerHeader
  { name :: Name.Name -- ^ Performer name
  , uid :: Uid.Uid -- ^ Performer unique ID
  }
  deriving (Eq, Show)

performerHeaderSchema :: FC.Fleece schema => schema PerformerHeader
performerHeaderSchema =
  FC.object $
    FC.constructor PerformerHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema