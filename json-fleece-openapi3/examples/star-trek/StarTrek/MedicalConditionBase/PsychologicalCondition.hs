{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionBase.PsychologicalCondition
  ( PsychologicalCondition(..)
  , psychologicalConditionSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PsychologicalCondition = PsychologicalCondition Bool
  deriving (Show, Eq)

psychologicalConditionSchema :: FC.Fleece schema => schema PsychologicalCondition
psychologicalConditionSchema =
  FC.coerceSchema FC.boolean