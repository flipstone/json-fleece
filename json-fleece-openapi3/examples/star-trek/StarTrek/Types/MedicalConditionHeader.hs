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
  { name :: Name.Name -- ^ Medical condition name
  , uid :: Uid.Uid -- ^ Medical condition unique ID
  }
  deriving (Eq, Show)

medicalConditionHeaderSchema :: FC.Fleece t => FC.Schema t MedicalConditionHeader
medicalConditionHeaderSchema =
  FC.object $
    FC.constructor MedicalConditionHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema