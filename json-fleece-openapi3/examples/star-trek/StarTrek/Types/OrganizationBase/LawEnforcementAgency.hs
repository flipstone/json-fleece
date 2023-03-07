{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBase.LawEnforcementAgency
  ( LawEnforcementAgency(..)
  , lawEnforcementAgencySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LawEnforcementAgency = LawEnforcementAgency Bool
  deriving (Show, Eq)

lawEnforcementAgencySchema :: FC.Fleece schema => schema LawEnforcementAgency
lawEnforcementAgencySchema =
  FC.coerceSchema FC.boolean