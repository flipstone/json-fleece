{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.StringParam
  ( StringParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype StringParam = StringParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition StringParam
paramDef =
  P.coerceParam (P.textParam "string-param")