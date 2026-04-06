{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.InlineEnumIntParam
  ( InlineEnumIntParam(..)
  , inlineEnumIntParamToText
  , inlineEnumIntParamFromText
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Either as Either
import qualified Data.Text as T
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data InlineEnumIntParam
  = InlineEnumIntParam10
  | InlineEnumIntParam20
  | InlineEnumIntParam30
  deriving (Eq, Show, Ord, Enum, Bounded)

inlineEnumIntParamToText :: InlineEnumIntParam -> T.Text
inlineEnumIntParamToText v =
  T.pack $
    case v of
      InlineEnumIntParam10 -> "10"
      InlineEnumIntParam20 -> "20"
      InlineEnumIntParam30 -> "30"

inlineEnumIntParamFromText :: T.Text -> Either String InlineEnumIntParam
inlineEnumIntParamFromText txt =
  case T.unpack txt of
    "10" -> Either.Right InlineEnumIntParam10
    "20" -> Either.Right InlineEnumIntParam20
    "30" -> Either.Right InlineEnumIntParam30
    v -> Either.Left $ "Unknown InlineEnumIntParam: " <> v

paramDef :: R.ParameterDefinition InlineEnumIntParam
paramDef =
  P.coerceParam (P.boundedEnumParam inlineEnumIntParamToText "inline-enum-int-param")