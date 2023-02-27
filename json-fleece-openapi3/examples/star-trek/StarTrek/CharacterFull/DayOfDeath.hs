{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.DayOfDeath
  ( DayOfDeath(..)
  , dayOfDeathSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype DayOfDeath = DayOfDeath Integer
  deriving (Show, Eq)

dayOfDeathSchema :: FC.Fleece schema => schema DayOfDeath
dayOfDeathSchema =
  FC.coerceSchema FC.integer