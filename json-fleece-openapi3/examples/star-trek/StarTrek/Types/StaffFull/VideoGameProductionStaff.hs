{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.VideoGameProductionStaff
  ( VideoGameProductionStaff(..)
  , videoGameProductionStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VideoGameProductionStaff = VideoGameProductionStaff Bool
  deriving (Show, Eq)

videoGameProductionStaffSchema :: FC.Fleece t => FC.Schema t VideoGameProductionStaff
videoGameProductionStaffSchema =
  FC.coerceSchema FC.boolean