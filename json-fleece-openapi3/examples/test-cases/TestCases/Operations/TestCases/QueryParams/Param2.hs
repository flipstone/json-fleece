{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams.Param2
  ( Param2(..)
  , def
  ) where

import qualified Beeline.Routing as R
import qualified Data.Text as T
import Prelude (Eq, Show)

newtype Param2 = Param2 T.Text
  deriving (Show, Eq)

def :: R.ParameterDefinition Param2
def =
  R.coerceParam (R.textParam "param2")