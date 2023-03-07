{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBase.IntergovernmentalOrganization
  ( IntergovernmentalOrganization(..)
  , intergovernmentalOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype IntergovernmentalOrganization = IntergovernmentalOrganization Bool
  deriving (Show, Eq)

intergovernmentalOrganizationSchema :: FC.Fleece schema => schema IntergovernmentalOrganization
intergovernmentalOrganizationSchema =
  FC.coerceSchema FC.boolean