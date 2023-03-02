{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.Estimate
  ( Estimate(..)
  , estimateSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Estimate = Estimate T.Text
  deriving (Show, Eq)

estimateSchema :: FC.Fleece schema => schema Estimate
estimateSchema =
  FC.coerceSchema FC.text