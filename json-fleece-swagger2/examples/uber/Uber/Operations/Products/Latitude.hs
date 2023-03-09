{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Uber.Operations.Products.Latitude
  ( Latitude(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import Prelude (Double, Eq, Show)

newtype Latitude = Latitude Double
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Latitude
paramDef =
  R.coerceParam (R.doubleParam "latitude")