{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.DateOfBirth
  ( DateOfBirth(..)
  , dateOfBirthSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DateOfBirth = DateOfBirth Day
  deriving (Show, Eq)

dateOfBirthSchema :: FC.Fleece schema => schema DateOfBirth
dateOfBirthSchema =
  FC.coerceSchema FC.day