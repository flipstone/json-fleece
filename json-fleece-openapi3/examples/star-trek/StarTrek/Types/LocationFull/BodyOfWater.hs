{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.BodyOfWater
  ( BodyOfWater(..)
  , bodyOfWaterSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BodyOfWater = BodyOfWater Bool
  deriving (Show, Eq)

bodyOfWaterSchema :: FC.Fleece schema => schema BodyOfWater
bodyOfWaterSchema =
  FC.coerceSchema FC.boolean