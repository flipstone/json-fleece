{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.BooleanParam
  ( BooleanParam(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import Prelude (Bool, Eq, Show)

newtype BooleanParam = BooleanParam Bool
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition BooleanParam
paramDef =
  R.coerceParam (R.booleanParam "boolean-param")