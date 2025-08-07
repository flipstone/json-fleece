{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.HeadersWithCookies.HeaderParam
  ( HeaderParam(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype HeaderParam = HeaderParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition HeaderParam
paramDef =
  P.coerceParam (P.textParam "header-param")