{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OrganizationFull.MilitaryUnit
  ( MilitaryUnit(..)
  , militaryUnitSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MilitaryUnit = MilitaryUnit Bool
  deriving (Show, Eq)

militaryUnitSchema :: FC.Fleece schema => schema MilitaryUnit
militaryUnitSchema =
  FC.coerceSchema FC.boolean