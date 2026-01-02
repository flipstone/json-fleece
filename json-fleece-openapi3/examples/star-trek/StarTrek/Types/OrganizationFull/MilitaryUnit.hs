{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.MilitaryUnit
  ( MilitaryUnit(..)
  , militaryUnitSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MilitaryUnit = MilitaryUnit Bool
  deriving (Show, Eq)

militaryUnitSchema :: FC.Fleece t => FC.Schema t MilitaryUnit
militaryUnitSchema =
  FC.coerceSchema FC.boolean