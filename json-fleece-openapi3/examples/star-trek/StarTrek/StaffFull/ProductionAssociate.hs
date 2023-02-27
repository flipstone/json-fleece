{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.ProductionAssociate
  ( ProductionAssociate(..)
  , productionAssociateSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionAssociate = ProductionAssociate Bool
  deriving (Show, Eq)

productionAssociateSchema :: FC.Fleece schema => schema ProductionAssociate
productionAssociateSchema =
  FC.coerceSchema FC.boolean