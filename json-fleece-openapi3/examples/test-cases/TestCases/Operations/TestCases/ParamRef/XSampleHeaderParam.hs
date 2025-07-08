{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.ParamRef.XSampleHeaderParam
  ( XSampleHeaderParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import Prelude (Bool, Eq, Show)

newtype XSampleHeaderParam = XSampleHeaderParam Bool
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition XSampleHeaderParam
paramDef =
  P.coerceParam (P.booleanParam "X-Sample-Header-Param")