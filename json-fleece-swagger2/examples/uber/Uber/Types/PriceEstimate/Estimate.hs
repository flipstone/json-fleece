{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.PriceEstimate.Estimate
  ( Estimate(..)
  , estimateSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Estimate = Estimate T.Text
  deriving (Show, Eq)

estimateSchema :: FC.Fleece t => FC.Schema t Estimate
estimateSchema =
  FC.coerceSchema FC.text