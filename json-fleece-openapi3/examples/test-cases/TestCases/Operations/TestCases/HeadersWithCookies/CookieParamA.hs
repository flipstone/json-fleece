{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.HeadersWithCookies.CookieParamA
  ( CookieParamA(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype CookieParamA = CookieParamA T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition CookieParamA
paramDef =
  P.coerceParam (P.textParam "cookie-param-a")