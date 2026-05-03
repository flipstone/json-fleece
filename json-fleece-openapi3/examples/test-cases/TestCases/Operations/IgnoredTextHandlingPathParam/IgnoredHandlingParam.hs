{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.IgnoredTextHandlingPathParam.IgnoredHandlingParam
  ( paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified TestCases.Types.IgnoredTextLengthType as IgnoredTextLengthType

paramDef :: R.ParameterDefinition IgnoredTextLengthType.IgnoredTextLengthType
paramDef =
  P.coerceParam (P.textParam "ignored-handling-param")