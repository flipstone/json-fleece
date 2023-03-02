{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationFull.MilitaryOrganization
  ( MilitaryOrganization(..)
  , militaryOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MilitaryOrganization = MilitaryOrganization Bool
  deriving (Show, Eq)

militaryOrganizationSchema :: FC.Fleece schema => schema MilitaryOrganization
militaryOrganizationSchema =
  FC.coerceSchema FC.boolean