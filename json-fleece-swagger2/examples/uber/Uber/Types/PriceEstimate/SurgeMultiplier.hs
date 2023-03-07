{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.PriceEstimate.SurgeMultiplier
  ( SurgeMultiplier(..)
  , surgeMultiplierSchema
  ) where

import qualified Data.Scientific as Sci
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SurgeMultiplier = SurgeMultiplier Sci.Scientific
  deriving (Show, Eq)

surgeMultiplierSchema :: FC.Fleece schema => schema SurgeMultiplier
surgeMultiplierSchema =
  FC.coerceSchema FC.number