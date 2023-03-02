{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationBase.MilitaryUnit
  ( MilitaryUnit(..)
  , militaryUnitSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MilitaryUnit = MilitaryUnit Bool
  deriving (Show, Eq)

militaryUnitSchema :: FC.Fleece schema => schema MilitaryUnit
militaryUnitSchema =
  FC.coerceSchema FC.boolean