{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.MakeupStaff
  ( MakeupStaff(..)
  , makeupStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MakeupStaff = MakeupStaff Bool
  deriving (Show, Eq)

makeupStaffSchema :: FC.Fleece t => FC.Schema t MakeupStaff
makeupStaffSchema =
  FC.coerceSchema FC.boolean