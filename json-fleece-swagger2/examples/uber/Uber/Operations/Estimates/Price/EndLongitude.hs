{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Price.EndLongitude
  ( EndLongitude(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype EndLongitude = EndLongitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition EndLongitude
paramDef =
  R.coerceParam (R.doubleParam "end_longitude")