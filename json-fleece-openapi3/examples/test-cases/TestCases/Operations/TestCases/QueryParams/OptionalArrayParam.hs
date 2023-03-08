{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.OptionalArrayParam
  ( OptionalArrayParam(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype OptionalArrayParam = OptionalArrayParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition OptionalArrayParam
paramDef =
  R.coerceParam (R.textParam "optional-array-param")