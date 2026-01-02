{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ProductionDesigner
  ( ProductionDesigner(..)
  , productionDesignerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionDesigner = ProductionDesigner Bool
  deriving (Show, Eq)

productionDesignerSchema :: FC.Fleece t => FC.Schema t ProductionDesigner
productionDesignerSchema =
  FC.coerceSchema FC.boolean