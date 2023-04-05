{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.OperationTypeOptions.PathParam.PathParam
  ( PathParam(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype PathParam = PathParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition PathParam
paramDef =
  R.coerceParam (R.textParam "path-param")