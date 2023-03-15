{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReference.EnumIntParam
  ( paramDef
  ) where

import qualified Beeline.Routing as R
import qualified TestCases.Types.EnumIntParam as EnumIntParam

paramDef :: R.ParameterDefinition EnumIntParam.EnumIntParam
paramDef =
  R.coerceParam (R.boundedEnumParam EnumIntParam.enumIntParamToText "enum-int-param")