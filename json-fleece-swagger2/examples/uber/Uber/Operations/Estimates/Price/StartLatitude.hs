{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Price.StartLatitude
  ( StartLatitude(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype StartLatitude = StartLatitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition StartLatitude
paramDef =
  R.coerceParam (R.doubleParam "start_latitude")