{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetMultiplePathsParams.Param1
  ( Param1(..)
  , paramDef
  ) where

import qualified Beeline.Params as P
import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Param1 = Param1 T.Text
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Param1
paramDef =
  P.coerceParam (P.textParam "param1")