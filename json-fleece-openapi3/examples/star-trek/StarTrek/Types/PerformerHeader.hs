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
  { uid :: Uid.Uid -- ^ Performer unique ID
  , name :: Name.Name -- ^ Performer name
  }
  deriving (Eq, Show)

performerHeaderSchema :: FC.Fleece schema => schema PerformerHeader
performerHeaderSchema =
  FC.object $
    FC.constructor PerformerHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema