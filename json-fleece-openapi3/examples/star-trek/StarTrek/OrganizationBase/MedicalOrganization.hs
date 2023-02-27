{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBase.MedicalOrganization
  ( MedicalOrganization(..)
  , medicalOrganizationSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MedicalOrganization = MedicalOrganization Bool
  deriving (Show, Eq)

medicalOrganizationSchema :: FC.Fleece schema => schema MedicalOrganization
medicalOrganizationSchema =
  FC.coerceSchema FC.boolean