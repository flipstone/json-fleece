{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.AssistantOrSecondUnitDirector
  ( AssistantOrSecondUnitDirector(..)
  , assistantOrSecondUnitDirectorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AssistantOrSecondUnitDirector = AssistantOrSecondUnitDirector Bool
  deriving (Show, Eq)

assistantOrSecondUnitDirectorSchema :: FC.Fleece t => FC.Schema t AssistantOrSecondUnitDirector
assistantOrSecondUnitDirectorSchema =
  FC.coerceSchema FC.boolean