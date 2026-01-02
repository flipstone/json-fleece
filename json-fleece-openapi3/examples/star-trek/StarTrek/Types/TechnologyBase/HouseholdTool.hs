{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.HouseholdTool
  ( HouseholdTool(..)
  , householdToolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HouseholdTool = HouseholdTool Bool
  deriving (Show, Eq)

householdToolSchema :: FC.Fleece t => FC.Schema t HouseholdTool
householdToolSchema =
  FC.coerceSchema FC.boolean