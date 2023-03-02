{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.HighEstimate
  ( HighEstimate(..)
  , highEstimateSchema
  ) where

import qualified Data.Scientific as Sci
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HighEstimate = HighEstimate Sci.Scientific
  deriving (Show, Eq)

highEstimateSchema :: FC.Fleece schema => schema HighEstimate
highEstimateSchema =
  FC.coerceSchema FC.number