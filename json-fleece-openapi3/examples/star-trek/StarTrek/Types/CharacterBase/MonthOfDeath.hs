{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.MonthOfDeath
  ( MonthOfDeath(..)
  , monthOfDeathSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype MonthOfDeath = MonthOfDeath Integer
  deriving (Show, Eq)

monthOfDeathSchema :: FC.Fleece t => FC.Schema t MonthOfDeath
monthOfDeathSchema =
  FC.coerceSchema FC.integer