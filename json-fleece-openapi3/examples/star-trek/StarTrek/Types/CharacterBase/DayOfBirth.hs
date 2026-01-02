{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.DayOfBirth
  ( DayOfBirth(..)
  , dayOfBirthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype DayOfBirth = DayOfBirth Integer
  deriving (Show, Eq)

dayOfBirthSchema :: FC.Fleece t => FC.Schema t DayOfBirth
dayOfBirthSchema =
  FC.coerceSchema FC.integer