{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.Estimate
  ( Estimate(..)
  , estimateSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Estimate = Estimate Text
  deriving (Show, Eq)

estimateSchema :: FC.Fleece schema => schema Estimate
estimateSchema =
  FC.coerceSchema FC.text