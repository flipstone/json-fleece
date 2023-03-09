{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Time.StartLongitude
  ( StartLongitude(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype StartLongitude = StartLongitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition StartLongitude
paramDef =
  R.coerceParam (R.doubleParam "start_longitude")