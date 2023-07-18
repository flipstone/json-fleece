{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MedicalConditionHeader
  ( MedicalConditionHeader(..)
  , medicalConditionHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.MedicalConditionHeader.Name as Name
import qualified StarTrek.Types.MedicalConditionHeader.Uid as Uid

data MedicalConditionHeader = MedicalConditionHeader
  { uid :: Uid.Uid -- ^ Medical condition unique ID
  , name :: Name.Name -- ^ Medical condition name
  }
  deriving (Eq, Show)

medicalConditionHeaderSchema :: FC.Fleece schema => schema MedicalConditionHeader
medicalConditionHeaderSchema =
  FC.object $
    FC.constructor MedicalConditionHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema