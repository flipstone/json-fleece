{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.MonthOfBirth
  ( MonthOfBirth(..)
  , monthOfBirthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype MonthOfBirth = MonthOfBirth Integer
  deriving (Show, Eq)

monthOfBirthSchema :: FC.Fleece t => FC.Schema t MonthOfBirth
monthOfBirthSchema =
  FC.coerceSchema FC.integer