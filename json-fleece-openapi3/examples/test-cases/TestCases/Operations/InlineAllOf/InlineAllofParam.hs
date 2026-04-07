{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.InlineAllOf.InlineAllofParam
  ( InlineAllofParam(..)
  , inlineAllofParamToText
  , inlineAllofParamFromText
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Either as Either
import qualified Data.Text as T
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

data InlineAllofParam
  = InlineAllofParam100
  | InlineAllofParam200
  | InlineAllofParam300
  | Green
  deriving (Eq, Show, Ord, Enum, Bounded)

inlineAllofParamToText :: InlineAllofParam -> T.Text
inlineAllofParamToText v =
  T.pack $
    case v of
      InlineAllofParam100 -> "100"
      InlineAllofParam200 -> "200"
      InlineAllofParam300 -> "300"
      Green -> "Green"

inlineAllofParamFromText :: T.Text -> Either.Either String InlineAllofParam
inlineAllofParamFromText txt =
  case T.unpack txt of
    "100" -> Either.Right InlineAllofParam100
    "200" -> Either.Right InlineAllofParam200
    "300" -> Either.Right InlineAllofParam300
    "Green" -> Either.Right Green
    v -> Either.Left $ "Unknown InlineAllofParam: " <> v

paramDef :: R.ParameterDefinition InlineAllofParam
paramDef =
  P.coerceParam (P.boundedEnumParam inlineAllofParamToText "inline-allof-param")