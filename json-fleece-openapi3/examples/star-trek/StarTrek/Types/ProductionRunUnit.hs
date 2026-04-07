{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ProductionRunUnit
  ( ProductionRunUnit(..)
  , productionRunUnitSchema
  , productionRunUnitToText
  , productionRunUnitFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

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

productionRunUnitFromText :: T.Text -> Either.Either String ProductionRunUnit
productionRunUnitFromText txt =
  case T.unpack txt of
    "BOX" -> Either.Right BOX
    "SET" -> Either.Right SET
    v -> Either.Left $ "Unknown ProductionRunUnit: " <> v

productionRunUnitSchema :: FC.Fleece t => FC.Schema t ProductionRunUnit
productionRunUnitSchema =
  FC.boundedEnum productionRunUnitToText