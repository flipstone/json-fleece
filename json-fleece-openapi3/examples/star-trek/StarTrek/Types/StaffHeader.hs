{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffHeader
  ( StaffHeader(..)
  , staffHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.StaffHeader.Name as Name
import qualified StarTrek.Types.StaffHeader.Uid as Uid

data StaffHeader = StaffHeader
  { uid :: Uid.Uid -- ^ Staff unique ID
  , name :: Name.Name -- ^ Staff name
  }
  deriving (Eq, Show)

staffHeaderSchema :: FC.Fleece schema => schema StaffHeader
staffHeaderSchema =
  FC.object $
    FC.constructor StaffHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema