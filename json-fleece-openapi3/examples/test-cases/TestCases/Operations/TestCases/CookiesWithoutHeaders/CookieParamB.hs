{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.CookiesWithoutHeaders.CookieParamB
  ( CookieParamB(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype CookieParamB = CookieParamB T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition CookieParamB
paramDef =
  P.coerceParam (P.textParam "cookie-param-b")