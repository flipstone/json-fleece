{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.YearOfBirth
  ( YearOfBirth(..)
  , yearOfBirthSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearOfBirth = YearOfBirth Integer
  deriving (Show, Eq)

yearOfBirthSchema :: FC.Fleece schema => schema YearOfBirth
yearOfBirthSchema =
  FC.coerceSchema FC.integer