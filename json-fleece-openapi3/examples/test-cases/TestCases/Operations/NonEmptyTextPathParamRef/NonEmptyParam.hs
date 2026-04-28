{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.NonEmptyTextPathParamRef.NonEmptyParam
  ( paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified TestCases.Types.NonEmptyTextType as NonEmptyTextType

paramDef :: R.ParameterDefinition NonEmptyTextType.NonEmptyTextType
paramDef =
  P.coerceParam (P.nonEmptyTextParam "non-empty-param")