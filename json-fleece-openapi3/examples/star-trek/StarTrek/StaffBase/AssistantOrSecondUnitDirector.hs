{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.AssistantOrSecondUnitDirector
  ( AssistantOrSecondUnitDirector(..)
  , assistantOrSecondUnitDirectorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AssistantOrSecondUnitDirector = AssistantOrSecondUnitDirector Bool
  deriving (Show, Eq)

assistantOrSecondUnitDirectorSchema :: FC.Fleece schema => schema AssistantOrSecondUnitDirector
assistantOrSecondUnitDirectorSchema =
  FC.coerceSchema FC.boolean