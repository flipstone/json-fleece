{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBase.DayOfBirth
  ( DayOfBirth(..)
  , dayOfBirthSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype DayOfBirth = DayOfBirth Integer
  deriving (Show, Eq)

dayOfBirthSchema :: FC.Fleece schema => schema DayOfBirth
dayOfBirthSchema =
  FC.coerceSchema FC.integer