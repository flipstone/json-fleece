{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.InlineEnumParam
  ( InlineEnumParam(..)
  , inlineEnumParamToText
  , inlineEnumParamFromText
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Either as Either
import qualified Data.Text as T
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data InlineEnumParam
  = Baz
  | Bat
  | Bax
  deriving (Eq, Show, Ord, Enum, Bounded)

inlineEnumParamToText :: InlineEnumParam -> T.Text
inlineEnumParamToText v =
  T.pack $
    case v of
      Baz -> "baz"
      Bat -> "bat"
      Bax -> "bax"

inlineEnumParamFromText :: T.Text -> Either String InlineEnumParam
inlineEnumParamFromText txt =
  case T.unpack txt of
    "baz" -> Either.Right Baz
    "bat" -> Either.Right Bat
    "bax" -> Either.Right Bax
    v -> Either.Left $ "Unknown InlineEnumParam: " <> v

paramDef :: R.ParameterDefinition InlineEnumParam
paramDef =
  P.coerceParam (P.boundedEnumParam inlineEnumParamToText "inline-enum-param")