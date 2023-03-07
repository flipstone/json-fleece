{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.DateOfBirth
  ( DateOfBirth(..)
  , dateOfBirthSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DateOfBirth = DateOfBirth Time.Day
  deriving (Show, Eq)

dateOfBirthSchema :: FC.Fleece schema => schema DateOfBirth
dateOfBirthSchema =
  FC.coerceSchema FC.day