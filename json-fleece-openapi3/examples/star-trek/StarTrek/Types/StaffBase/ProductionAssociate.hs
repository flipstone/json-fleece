{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ProductionAssociate
  ( ProductionAssociate(..)
  , productionAssociateSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionAssociate = ProductionAssociate Bool
  deriving (Show, Eq)

productionAssociateSchema :: FC.Fleece t => FC.Schema t ProductionAssociate
productionAssociateSchema =
  FC.coerceSchema FC.boolean