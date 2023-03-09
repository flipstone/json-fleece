{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Estimates.Price.EndLatitude
  ( EndLatitude(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype EndLatitude = EndLatitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition EndLatitude
paramDef =
  R.coerceParam (R.doubleParam "end_latitude")