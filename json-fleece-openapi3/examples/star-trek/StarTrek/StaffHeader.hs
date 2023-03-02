{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffHeader
  ( StaffHeader(..)
  , staffHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.StaffHeader.Name as Name
import qualified StarTrek.StaffHeader.Uid as Uid

data StaffHeader = StaffHeader
  { name :: Name.Name -- ^ Staff name
  , uid :: Uid.Uid -- ^ Staff unique ID
  }
  deriving (Eq, Show)

staffHeaderSchema :: FC.Fleece schema => schema StaffHeader
staffHeaderSchema =
  FC.object $
    FC.constructor StaffHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema