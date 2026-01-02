{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.MedicalOrganization
  ( MedicalOrganization(..)
  , medicalOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MedicalOrganization = MedicalOrganization Bool
  deriving (Show, Eq)

medicalOrganizationSchema :: FC.Fleece t => FC.Schema t MedicalOrganization
medicalOrganizationSchema =
  FC.coerceSchema FC.boolean