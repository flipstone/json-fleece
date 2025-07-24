{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Time.StartLongitude
  ( StartLongitude(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype StartLongitude = StartLongitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition StartLongitude
paramDef =
  P.coerceParam (P.doubleParam "start_longitude")