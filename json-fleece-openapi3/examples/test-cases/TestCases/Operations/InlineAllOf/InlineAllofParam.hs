{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.InlineAllOf.InlineAllofParam
  ( InlineAllofParam(..)
  , inlineAllofParamToText
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

paramDef :: R.ParameterDefinition InlineAllofParam
paramDef =
  P.coerceParam (P.boundedEnumParam inlineAllofParamToText "inline-allof-param")