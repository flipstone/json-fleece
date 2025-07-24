{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Price.EndLongitude
  ( EndLongitude(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype EndLongitude = EndLongitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition EndLongitude
paramDef =
  P.coerceParam (P.doubleParam "end_longitude")