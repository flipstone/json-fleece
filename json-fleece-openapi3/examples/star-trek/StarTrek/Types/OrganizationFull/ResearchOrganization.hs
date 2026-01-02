{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.ResearchOrganization
  ( ResearchOrganization(..)
  , researchOrganizationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ResearchOrganization = ResearchOrganization Bool
  deriving (Show, Eq)

researchOrganizationSchema :: FC.Fleece t => FC.Schema t ResearchOrganization
researchOrganizationSchema =
  FC.coerceSchema FC.boolean