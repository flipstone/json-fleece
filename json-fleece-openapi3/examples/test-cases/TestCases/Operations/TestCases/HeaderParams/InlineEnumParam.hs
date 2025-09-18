{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.HeaderParams.InlineEnumParam
  ( InlineEnumParam(..)
  , inlineEnumParamToText
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

paramDef :: R.ParameterDefinition InlineEnumParam
paramDef =
  P.coerceParam (P.boundedEnumParam inlineEnumParamToText "inline-enum-param")