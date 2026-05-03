{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.NonEmptyTextHandlingPathParam.NonEmptyHandlingParam
  ( NonEmptyHandlingParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.NonEmptyText as NET
import Prelude (Eq, Show)

newtype NonEmptyHandlingParam = NonEmptyHandlingParam NET.NonEmptyText
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition NonEmptyHandlingParam
paramDef =
  P.coerceParam (P.nonEmptyTextParam "non-empty-handling-param")