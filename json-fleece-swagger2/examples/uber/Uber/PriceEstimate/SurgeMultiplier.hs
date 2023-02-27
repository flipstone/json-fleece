{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.SurgeMultiplier
  ( SurgeMultiplier(..)
  , surgeMultiplierSchema
  ) where

import Data.Scientific (Scientific)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SurgeMultiplier = SurgeMultiplier Scientific
  deriving (Show, Eq)

surgeMultiplierSchema :: FC.Fleece schema => schema SurgeMultiplier
surgeMultiplierSchema =
  FC.coerceSchema FC.number