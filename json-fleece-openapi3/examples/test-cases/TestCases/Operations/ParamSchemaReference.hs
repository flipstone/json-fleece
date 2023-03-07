{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReference
  ( ParamSchemaReference(..)
  , route
  ) where

import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)
import qualified TestCases.Operations.ParamSchemaReference.StringParam as StringParam
import qualified TestCases.Types.StringParam as StringParam

data ParamSchemaReference = ParamSchemaReference
  { stringParam :: StringParam.StringParam
  }
  deriving (Eq, Show)

route :: R.Router r => r ParamSchemaReference
route =
  R.get $
    R.make ParamSchemaReference
      /- "test-cases"
      /- "param-ref"
      /+ R.Param StringParam.paramDef stringParam