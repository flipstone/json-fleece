{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Operations.BoundedTextPathParamRef.BoundedParam
  ( paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified TestCases.Types.BoundedTextType as BoundedTextType

paramDef :: R.ParameterDefinition BoundedTextType.BoundedTextType
paramDef =
  P.coerceParam (P.boundedTextParam "bounded-param")