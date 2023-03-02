{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyBase.EngineeringTool
  ( EngineeringTool(..)
  , engineeringToolSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EngineeringTool = EngineeringTool Bool
  deriving (Show, Eq)

engineeringToolSchema :: FC.Fleece schema => schema EngineeringTool
engineeringToolSchema =
  FC.coerceSchema FC.boolean