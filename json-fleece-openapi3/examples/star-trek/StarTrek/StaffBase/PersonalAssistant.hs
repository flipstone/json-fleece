{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.PersonalAssistant
  ( PersonalAssistant(..)
  , personalAssistantSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PersonalAssistant = PersonalAssistant Bool
  deriving (Show, Eq)

personalAssistantSchema :: FC.Fleece schema => schema PersonalAssistant
personalAssistantSchema =
  FC.coerceSchema FC.boolean