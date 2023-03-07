{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.PriceEstimate.LowEstimate
  ( LowEstimate(..)
  , lowEstimateSchema
  ) where

import qualified Data.Scientific as Sci
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype LowEstimate = LowEstimate Sci.Scientific
  deriving (Show, Eq)

lowEstimateSchema :: FC.Fleece schema => schema LowEstimate
lowEstimateSchema =
  FC.coerceSchema FC.number