{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.PersonalAssistant
  ( PersonalAssistant(..)
  , personalAssistantSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PersonalAssistant = PersonalAssistant Bool
  deriving (Show, Eq)

personalAssistantSchema :: FC.Fleece t => FC.Schema t PersonalAssistant
personalAssistantSchema =
  FC.coerceSchema FC.boolean