{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerHeader
  ( PerformerHeader(..)
  , performerHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.PerformerHeader.Name as Name
import qualified StarTrek.Types.PerformerHeader.Uid as Uid

data PerformerHeader = PerformerHeader
  { name :: Name.Name -- ^ Performer name
  , uid :: Uid.Uid -- ^ Performer unique ID
  }
  deriving (Eq, Show)

performerHeaderSchema :: FC.Fleece t => FC.Schema t PerformerHeader
performerHeaderSchema =
  FC.object $
    FC.constructor PerformerHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema