{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBase.SportOrganization
  ( SportOrganization(..)
  , sportOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SportOrganization = SportOrganization Bool
  deriving (Show, Eq)

sportOrganizationSchema :: FC.Fleece schema => schema SportOrganization
sportOrganizationSchema =
  FC.coerceSchema FC.boolean