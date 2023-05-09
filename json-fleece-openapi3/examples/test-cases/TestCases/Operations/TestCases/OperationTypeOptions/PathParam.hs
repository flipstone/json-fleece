{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.OperationTypeOptions.PathParam
  ( operation
  , PathParams(..)
  , route
  , QueryParams(..)
  , queryParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import Prelude (($), fmap)
import qualified TestCases.Operations.TestCases.OperationTypeOptions.PathParam.PathParam as PathParam
import qualified TestCases.Operations.TestCases.OperationTypeOptions.PathParam.QueryParam as QueryParam

operation ::
  H.Operation
    H.ContentTypeDecodingError
    PathParams
    QueryParams
    H.NoHeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestQuerySchema = queryParamsSchema
    , H.responseSchemas = responseSchemas
    }

data PathParams = PathParams
  { pathParam :: PathParam.PathParam
  }
  deriving ()

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /- "operation-type-options"
      /+ R.Param PathParam.paramDef pathParam

data QueryParams = QueryParams
  { queryParam :: QueryParam.QueryParam
  }
  deriving ()

queryParamsSchema :: H.ParameterCollectionSchema p => p QueryParams QueryParams
queryParamsSchema =
  H.makeParams QueryParams
    ?+ H.required queryParam QueryParam.paramDef

data Responses
  = Response201 H.NoResponseBody
  | Response422 H.NoResponseBody
  deriving ()

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 201, fmap Response201 (H.noResponseBody))
  , (H.Status 422, fmap Response422 (H.noResponseBody))
  ]