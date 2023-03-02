{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.DateOfDeath
  ( DateOfDeath(..)
  , dateOfDeathSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DateOfDeath = DateOfDeath Time.Day
  deriving (Show, Eq)

dateOfDeathSchema :: FC.Fleece schema => schema DateOfDeath
dateOfDeathSchema =
  FC.coerceSchema FC.day