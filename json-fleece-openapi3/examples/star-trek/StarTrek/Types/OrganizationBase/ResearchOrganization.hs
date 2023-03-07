{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBase.ResearchOrganization
  ( ResearchOrganization(..)
  , researchOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ResearchOrganization = ResearchOrganization Bool
  deriving (Show, Eq)

researchOrganizationSchema :: FC.Fleece schema => schema ResearchOrganization
researchOrganizationSchema =
  FC.coerceSchema FC.boolean