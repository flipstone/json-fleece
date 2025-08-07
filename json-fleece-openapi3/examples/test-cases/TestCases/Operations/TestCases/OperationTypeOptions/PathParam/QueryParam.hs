{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.OperationTypeOptions.PathParam.QueryParam
  ( QueryParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype QueryParam = QueryParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition QueryParam
paramDef =
  P.coerceParam (P.textParam "query-param")