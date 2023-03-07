{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetMultiplePathsParams.Param2
  ( Param2(..)
  , def
  ) where

import qualified Beeline.Routing as R
import qualified Data.Int as I
import Prelude (Eq, Show)

newtype Param2 = Param2 I.Int32
  deriving (Show, Eq)

def :: R.ParameterDefinition Param2
def =
  R.coerceParam (R.int32Param "param2")