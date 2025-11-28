{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.InlineAllOf.XSampleAllOfParam
  ( XSampleAllOfParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype XSampleAllOfParam = XSampleAllOfParam I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition XSampleAllOfParam
paramDef =
  P.coerceParam (P.int32Param "X-Sample-AllOf-Param")