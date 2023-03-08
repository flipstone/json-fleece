{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetMultiplePathsParams
  ( operation
  , PathParams(..)
  , route
  , QueryParams(..)
  , queryParamsSchema
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)
import qualified TestCases.Operations.GetMultiplePathsParams.Param1 as Param1
import qualified TestCases.Operations.GetMultiplePathsParams.Param2 as Param2

operation ::
  H.Operation
    H.ContentTypeDecodingError
    PathParams
    QueryParams
    H.NoRequestBody
    H.NoResponseBody
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestQuerySchema = queryParamsSchema
    }

data PathParams = PathParams
  { param1 :: Param1.Param1
  , param2 :: Param2.Param2
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /+ R.Param Param1.paramDef param1
      /- "multiple-path-params"
      /+ R.Param Param2.paramDef param2

data QueryParams = QueryParams
  deriving (Eq, Show)

queryParamsSchema :: H.QuerySchema q => q QueryParams QueryParams
queryParamsSchema =
  H.makeQuery QueryParams