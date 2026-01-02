{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.DayOfDeath
  ( DayOfDeath(..)
  , dayOfDeathSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype DayOfDeath = DayOfDeath Integer
  deriving (Show, Eq)

dayOfDeathSchema :: FC.Fleece t => FC.Schema t DayOfDeath
dayOfDeathSchema =
  FC.coerceSchema FC.integer