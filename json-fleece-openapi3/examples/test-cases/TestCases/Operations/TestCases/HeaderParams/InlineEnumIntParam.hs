{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.HeaderParams.InlineEnumIntParam
  ( InlineEnumIntParam(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

paramDef :: R.ParameterDefinition InlineEnumIntParam
paramDef =
  R.coerceParam (R.boundedEnumParam inlineEnumIntParamToText "inline-enum-int-param")