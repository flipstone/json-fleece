{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.ParamSchemaReference
  ( PathParams(..)
  , route
  , QueryParams(..)
  , queryParamsSchema
  ) where

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Operations.ParamSchemaReference.EnumParam as EnumParam
import qualified TestCases.Operations.ParamSchemaReference.StringParam as StringParam
import qualified TestCases.Types.EnumParam as EnumParam
import qualified TestCases.Types.StringParam as StringParam

data PathParams = PathParams
  { stringParam :: StringParam.StringParam
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /- "param-ref"
      /+ R.Param StringParam.paramDef stringParam

data QueryParams = QueryParams
  { enumParam :: Maybe EnumParam.EnumParam
  }
  deriving (Eq, Show)

queryParamsSchema :: H.QuerySchema q => q QueryParams QueryParams
queryParamsSchema =
  H.makeQuery QueryParams
    ?+ H.optional enumParam EnumParam.paramDef