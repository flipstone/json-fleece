{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.HighEstimate
  ( HighEstimate(..)
  , highEstimateSchema
  ) where

import Data.Scientific (Scientific)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HighEstimate = HighEstimate Scientific
  deriving (Show, Eq)

highEstimateSchema :: FC.Fleece schema => schema HighEstimate
highEstimateSchema =
  FC.coerceSchema FC.number