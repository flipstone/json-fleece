{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.SchemaRefAliasPathParam.AliasParam
  ( paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified TestCases.Types.AStringType as AStringType

paramDef :: R.ParameterDefinition AStringType.AStringType
paramDef =
  P.coerceParam (P.textParam "alias-param")