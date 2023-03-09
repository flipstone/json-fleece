{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Products.Longitude
  ( Longitude(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype Longitude = Longitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Longitude
paramDef =
  R.coerceParam (R.doubleParam "longitude")