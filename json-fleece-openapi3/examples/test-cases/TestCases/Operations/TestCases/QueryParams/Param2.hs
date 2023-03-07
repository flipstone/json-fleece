{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.Param2
  ( Param2(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Param2 = Param2 T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Param2
paramDef =
  R.coerceParam (R.textParam "param2")