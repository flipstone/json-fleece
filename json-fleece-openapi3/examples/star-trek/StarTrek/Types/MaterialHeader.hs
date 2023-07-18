{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialHeader
  ( MaterialHeader(..)
  , materialHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.MaterialHeader.Name as Name
import qualified StarTrek.Types.MaterialHeader.Uid as Uid

data MaterialHeader = MaterialHeader
  { uid :: Uid.Uid -- ^ Material unique ID
  , name :: Name.Name -- ^ Material name
  }
  deriving (Eq, Show)

materialHeaderSchema :: FC.Fleece schema => schema MaterialHeader
materialHeaderSchema =
  FC.object $
    FC.constructor MaterialHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema