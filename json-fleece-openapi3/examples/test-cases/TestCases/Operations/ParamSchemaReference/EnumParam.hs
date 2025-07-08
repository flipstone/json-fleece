{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReference.EnumParam
  ( paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified TestCases.Types.EnumParam as EnumParam

paramDef :: R.ParameterDefinition EnumParam.EnumParam
paramDef =
  P.coerceParam (P.boundedEnumParam EnumParam.enumParamToText "enum-param")