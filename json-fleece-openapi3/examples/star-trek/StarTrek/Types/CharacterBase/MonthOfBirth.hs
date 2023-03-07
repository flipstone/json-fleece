{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.MonthOfBirth
  ( MonthOfBirth(..)
  , monthOfBirthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype MonthOfBirth = MonthOfBirth Integer
  deriving (Show, Eq)

monthOfBirthSchema :: FC.Fleece schema => schema MonthOfBirth
monthOfBirthSchema =
  FC.coerceSchema FC.integer