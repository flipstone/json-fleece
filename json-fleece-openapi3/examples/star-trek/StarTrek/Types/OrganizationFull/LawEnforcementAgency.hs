{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.LawEnforcementAgency
  ( LawEnforcementAgency(..)
  , lawEnforcementAgencySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LawEnforcementAgency = LawEnforcementAgency Bool
  deriving (Show, Eq)

lawEnforcementAgencySchema :: FC.Fleece t => FC.Schema t LawEnforcementAgency
lawEnforcementAgencySchema =
  FC.coerceSchema FC.boolean