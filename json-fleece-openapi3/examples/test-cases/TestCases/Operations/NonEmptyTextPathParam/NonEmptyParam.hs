{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.NonEmptyTextPathParam.NonEmptyParam
  ( NonEmptyParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.NonEmptyText as NET
import Prelude (Eq, Show)

newtype NonEmptyParam = NonEmptyParam NET.NonEmptyText
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition NonEmptyParam
paramDef =
  P.coerceParam (P.nonEmptyTextParam "non-empty-param")