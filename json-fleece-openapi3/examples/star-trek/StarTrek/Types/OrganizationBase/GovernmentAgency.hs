{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBase.GovernmentAgency
  ( GovernmentAgency(..)
  , governmentAgencySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GovernmentAgency = GovernmentAgency Bool
  deriving (Show, Eq)

governmentAgencySchema :: FC.Fleece schema => schema GovernmentAgency
governmentAgencySchema =
  FC.coerceSchema FC.boolean