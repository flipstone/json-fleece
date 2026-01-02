{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.SportOrganization
  ( SportOrganization(..)
  , sportOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SportOrganization = SportOrganization Bool
  deriving (Show, Eq)

sportOrganizationSchema :: FC.Fleece t => FC.Schema t SportOrganization
sportOrganizationSchema =
  FC.coerceSchema FC.boolean