{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.LowEstimate
  ( LowEstimate(..)
  , lowEstimateSchema
  ) where

import Data.Scientific (Scientific)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype LowEstimate = LowEstimate Scientific
  deriving (Show, Eq)

lowEstimateSchema :: FC.Fleece schema => schema LowEstimate
lowEstimateSchema =
  FC.coerceSchema FC.number