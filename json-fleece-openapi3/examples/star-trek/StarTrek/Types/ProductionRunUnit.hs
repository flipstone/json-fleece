{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ProductionRunUnit
  ( ProductionRunUnit(..)
  , productionRunUnitSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data ProductionRunUnit
  = BOX
  | SET
  deriving (Eq, Show, Ord, Enum, Bounded)

productionRunUnitToText :: ProductionRunUnit -> T.Text
productionRunUnitToText v =
  T.pack $
    case v of
      BOX -> "BOX"
      SET -> "SET"

productionRunUnitSchema :: FC.Fleece schema => schema ProductionRunUnit
productionRunUnitSchema =
  FC.boundedEnum productionRunUnitToText