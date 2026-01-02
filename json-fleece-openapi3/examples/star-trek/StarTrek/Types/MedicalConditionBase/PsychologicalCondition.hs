{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MedicalConditionBase.PsychologicalCondition
  ( PsychologicalCondition(..)
  , psychologicalConditionSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PsychologicalCondition = PsychologicalCondition Bool
  deriving (Show, Eq)

psychologicalConditionSchema :: FC.Fleece t => FC.Schema t PsychologicalCondition
psychologicalConditionSchema =
  FC.coerceSchema FC.boolean