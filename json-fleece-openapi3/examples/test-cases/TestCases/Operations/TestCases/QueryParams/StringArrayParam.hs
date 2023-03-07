{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.StringArrayParam
  ( StringArrayParam(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype StringArrayParam = StringArrayParam T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition StringArrayParam
paramDef =
  R.coerceParam (R.textParam "string-array-param")