{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ProductionStaff
  ( ProductionStaff(..)
  , productionStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionStaff = ProductionStaff Bool
  deriving (Show, Eq)

productionStaffSchema :: FC.Fleece schema => schema ProductionStaff
productionStaffSchema =
  FC.coerceSchema FC.boolean