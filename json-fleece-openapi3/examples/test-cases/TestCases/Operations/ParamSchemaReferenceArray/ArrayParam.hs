{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReferenceArray.ArrayParam
  ( paramDef
  ) where

import qualified Beeline.Routing as R
import qualified TestCases.Types.StringParam as StringParam

paramDef :: R.ParameterDefinition StringParam.StringParam
paramDef =
  R.coerceParam (R.textParam "array-param")