{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.MilitaryOrganization
  ( MilitaryOrganization(..)
  , militaryOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MilitaryOrganization = MilitaryOrganization Bool
  deriving (Show, Eq)

militaryOrganizationSchema :: FC.Fleece t => FC.Schema t MilitaryOrganization
militaryOrganizationSchema =
  FC.coerceSchema FC.boolean