{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.HeaderParams.InlineEnumParam
  ( InlineEnumParam(..)
  , paramDef
  ) where

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
  R.coerceParam (R.boundedEnumParam inlineEnumParamToText "inline-enum-param")