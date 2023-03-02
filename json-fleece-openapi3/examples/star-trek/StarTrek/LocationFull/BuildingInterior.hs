{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.BuildingInterior
  ( BuildingInterior(..)
  , buildingInteriorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BuildingInterior = BuildingInterior Bool
  deriving (Show, Eq)

buildingInteriorSchema :: FC.Fleece schema => schema BuildingInterior
buildingInteriorSchema =
  FC.coerceSchema FC.boolean