{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.YearOfBirth
  ( YearOfBirth(..)
  , yearOfBirthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearOfBirth = YearOfBirth Integer
  deriving (Show, Eq)

yearOfBirthSchema :: FC.Fleece t => FC.Schema t YearOfBirth
yearOfBirthSchema =
  FC.coerceSchema FC.integer