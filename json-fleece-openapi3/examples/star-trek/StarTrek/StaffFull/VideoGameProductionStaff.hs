{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.VideoGameProductionStaff
  ( VideoGameProductionStaff(..)
  , videoGameProductionStaffSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VideoGameProductionStaff = VideoGameProductionStaff Bool
  deriving (Show, Eq)

videoGameProductionStaffSchema :: FC.Fleece schema => schema VideoGameProductionStaff
videoGameProductionStaffSchema =
  FC.coerceSchema FC.boolean