{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.BuildingInterior
  ( BuildingInterior(..)
  , buildingInteriorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BuildingInterior = BuildingInterior Bool
  deriving (Show, Eq)

buildingInteriorSchema :: FC.Fleece t => FC.Schema t BuildingInterior
buildingInteriorSchema =
  FC.coerceSchema FC.boolean