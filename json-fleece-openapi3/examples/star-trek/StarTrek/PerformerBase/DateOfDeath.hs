{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase.DateOfDeath
  ( DateOfDeath(..)
  , dateOfDeathSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DateOfDeath = DateOfDeath Day
  deriving (Show, Eq)

dateOfDeathSchema :: FC.Fleece schema => schema DateOfDeath
dateOfDeathSchema =
  FC.coerceSchema FC.day