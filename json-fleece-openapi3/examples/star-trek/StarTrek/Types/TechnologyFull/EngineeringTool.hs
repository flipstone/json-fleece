{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.EngineeringTool
  ( EngineeringTool(..)
  , engineeringToolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EngineeringTool = EngineeringTool Bool
  deriving (Show, Eq)

engineeringToolSchema :: FC.Fleece t => FC.Schema t EngineeringTool
engineeringToolSchema =
  FC.coerceSchema FC.boolean