{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetMultiplePathsParams
  ( GetMultiplePathsParams(..)
  , route
  ) where

import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)
import qualified TestCases.Operations.GetMultiplePathsParams.Param1 as Param1
import qualified TestCases.Operations.GetMultiplePathsParams.Param2 as Param2

data GetMultiplePathsParams = GetMultiplePathsParams
  { param1 :: Param1.Param1
  , param2 :: Param2.Param2
  }
  deriving (Eq, Show)

route :: R.Router r => r GetMultiplePathsParams
route =
  R.get $
    R.make GetMultiplePathsParams
      /- "test-cases"
      /+ R.Param Param1.paramDef param1
      /- "multiple-path-params"
      /+ R.Param Param2.paramDef param2