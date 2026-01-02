{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ProductionStaff
  ( ProductionStaff(..)
  , productionStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionStaff = ProductionStaff Bool
  deriving (Show, Eq)

productionStaffSchema :: FC.Fleece t => FC.Schema t ProductionStaff
productionStaffSchema =
  FC.coerceSchema FC.boolean