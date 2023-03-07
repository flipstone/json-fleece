{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.DayOfDeath
  ( DayOfDeath(..)
  , dayOfDeathSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype DayOfDeath = DayOfDeath Integer
  deriving (Show, Eq)

dayOfDeathSchema :: FC.Fleece schema => schema DayOfDeath
dayOfDeathSchema =
  FC.coerceSchema FC.integer