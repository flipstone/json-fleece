{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull.Fuel
  ( Fuel(..)
  , fuelSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Fuel = Fuel Bool
  deriving (Show, Eq)

fuelSchema :: FC.Fleece schema => schema Fuel
fuelSchema =
  FC.coerceSchema FC.boolean