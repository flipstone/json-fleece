{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.HouseholdTool
  ( HouseholdTool(..)
  , householdToolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HouseholdTool = HouseholdTool Bool
  deriving (Show, Eq)

householdToolSchema :: FC.Fleece schema => schema HouseholdTool
householdToolSchema =
  FC.coerceSchema FC.boolean