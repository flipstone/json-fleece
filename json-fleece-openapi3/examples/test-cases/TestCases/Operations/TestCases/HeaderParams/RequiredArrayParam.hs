{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.HeaderParams.RequiredArrayParam
  ( RequiredArrayParam(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype RequiredArrayParam = RequiredArrayParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition RequiredArrayParam
paramDef =
  R.coerceParam (R.textParam "required-array-param")