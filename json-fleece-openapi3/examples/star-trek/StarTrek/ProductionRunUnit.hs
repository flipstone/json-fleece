{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ProductionRunUnit
  ( ProductionRunUnit(..)
  , productionRunUnitSchema
  ) where

import Data.Text (Text, pack)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data ProductionRunUnit
  = BOX
  | SET
  deriving (Eq, Show, Ord, Enum, Bounded)

productionRunUnitToText :: ProductionRunUnit -> Text
productionRunUnitToText v =
  pack $
    case v of
      BOX -> "BOX"
      SET -> "SET"

productionRunUnitSchema :: FC.Fleece schema => schema ProductionRunUnit
productionRunUnitSchema =
  FC.boundedEnum productionRunUnitToText