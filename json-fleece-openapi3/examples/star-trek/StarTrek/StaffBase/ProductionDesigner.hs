{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ProductionDesigner
  ( ProductionDesigner(..)
  , productionDesignerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ProductionDesigner = ProductionDesigner Bool
  deriving (Show, Eq)

productionDesignerSchema :: FC.Fleece schema => schema ProductionDesigner
productionDesignerSchema =
  FC.coerceSchema FC.boolean