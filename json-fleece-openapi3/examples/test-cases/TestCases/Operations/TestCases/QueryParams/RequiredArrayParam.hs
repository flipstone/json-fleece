{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.RequiredArrayParam
  ( RequiredArrayParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype RequiredArrayParam = RequiredArrayParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition RequiredArrayParam
paramDef =
  P.coerceParam (P.textParam "required-array-param")