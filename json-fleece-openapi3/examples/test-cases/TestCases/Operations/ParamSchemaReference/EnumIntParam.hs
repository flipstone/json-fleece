{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReference.EnumIntParam
  ( paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified TestCases.Types.EnumIntParam as EnumIntParam

paramDef :: R.ParameterDefinition EnumIntParam.EnumIntParam
paramDef =
  P.coerceParam (P.boundedEnumParam EnumIntParam.enumIntParamToText "enum-int-param")