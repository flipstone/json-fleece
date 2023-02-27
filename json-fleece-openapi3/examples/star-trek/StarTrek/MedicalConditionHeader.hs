{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionHeader
  ( MedicalConditionHeader(..)
  , medicalConditionHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.MedicalConditionHeader.Name (Name, nameSchema)
import StarTrek.MedicalConditionHeader.Uid (Uid, uidSchema)

data MedicalConditionHeader = MedicalConditionHeader
  { name :: Name -- ^ Medical condition name
  , uid :: Uid -- ^ Medical condition unique ID
  }
  deriving (Eq, Show)

medicalConditionHeaderSchema :: FC.Fleece schema => schema MedicalConditionHeader
medicalConditionHeaderSchema =
  FC.object $
    FC.constructor MedicalConditionHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema