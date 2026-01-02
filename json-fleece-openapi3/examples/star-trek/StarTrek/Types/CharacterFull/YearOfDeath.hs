{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.YearOfDeath
  ( YearOfDeath(..)
  , yearOfDeathSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearOfDeath = YearOfDeath Integer
  deriving (Show, Eq)

yearOfDeathSchema :: FC.Fleece t => FC.Schema t YearOfDeath
yearOfDeathSchema =
  FC.coerceSchema FC.integer