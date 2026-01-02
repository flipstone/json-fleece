{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.GovernmentAgency
  ( GovernmentAgency(..)
  , governmentAgencySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GovernmentAgency = GovernmentAgency Bool
  deriving (Show, Eq)

governmentAgencySchema :: FC.Fleece t => FC.Schema t GovernmentAgency
governmentAgencySchema =
  FC.coerceSchema FC.boolean