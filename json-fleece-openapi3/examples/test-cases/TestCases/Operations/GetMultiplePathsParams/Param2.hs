{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetMultiplePathsParams.Param2
  ( Param2(..)
  , paramDef
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype Param2 = Param2 I.Int32
  deriving (Show, Eq)

paramDef :: R.ParameterDefinition Param2
paramDef =
  R.coerceParam (R.int32Param "param2")