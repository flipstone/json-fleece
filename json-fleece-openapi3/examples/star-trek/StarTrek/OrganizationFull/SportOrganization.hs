{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationFull.SportOrganization
  ( SportOrganization(..)
  , sportOrganizationSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SportOrganization = SportOrganization Bool
  deriving (Show, Eq)

sportOrganizationSchema :: FC.Fleece schema => schema SportOrganization
sportOrganizationSchema =
  FC.coerceSchema FC.boolean