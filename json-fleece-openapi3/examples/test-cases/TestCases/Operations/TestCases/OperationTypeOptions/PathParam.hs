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

import qualified Beeline.HTTP.Client as H
import Beeline.Params ((?+))
import qualified Beeline.Params as P
import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), fmap)
import qualified TestCases.Operations.TestCases.OperationTypeOptions.PathParam.PathParam as PathParam
import qualified TestCases.Operations.TestCases.OperationTypeOptions.PathParam.QueryParam as QueryParam

operation ::
  H.Operation
    FA.JSONDecodingError
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

queryParamsSchema :: P.QuerySchema p => p QueryParams QueryParams
queryParamsSchema =
  P.makeParams QueryParams
    ?+ P.required queryParam QueryParam.paramDef

data Responses
  = Response201 H.NoResponseBody
  | Response422 H.NoResponseBody
  deriving ()

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 201, fmap Response201 (H.noResponseBody))
  , (H.Status 422, fmap Response422 (H.noResponseBody))
  ]