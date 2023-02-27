{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBase.BodyOfWater
  ( BodyOfWater(..)
  , bodyOfWaterSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BodyOfWater = BodyOfWater Bool
  deriving (Show, Eq)

bodyOfWaterSchema :: FC.Fleece schema => schema BodyOfWater
bodyOfWaterSchema =
  FC.coerceSchema FC.boolean