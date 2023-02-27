{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffHeader
  ( StaffHeader(..)
  , staffHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.StaffHeader.Name (Name, nameSchema)
import StarTrek.StaffHeader.Uid (Uid, uidSchema)

data StaffHeader = StaffHeader
  { name :: Name -- ^ Staff name
  , uid :: Uid -- ^ Staff unique ID
  }
  deriving (Eq, Show)

staffHeaderSchema :: FC.Fleece schema => schema StaffHeader
staffHeaderSchema =
  FC.object $
    FC.constructor StaffHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema