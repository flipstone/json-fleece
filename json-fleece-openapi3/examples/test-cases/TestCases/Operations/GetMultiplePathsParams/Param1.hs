{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetMultiplePathsParams.Param1
  ( Param1(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Param1 = Param1 T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Param1
paramDef =
  R.coerceParam (R.textParam "param1")