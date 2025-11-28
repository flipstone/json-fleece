{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.InlineAllOf
  ( operation
  , route
  , QueryParams(..)
  , queryParamsSchema
  , HeaderParams(..)
  , headerParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Params ((?+))
import qualified Beeline.Params as P
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Show, fmap)
import qualified TestCases.Operations.InlineAllOf.InlineAllofParam as InlineAllofParam
import qualified TestCases.Operations.InlineAllOf.RequestBody as RequestBody
import qualified TestCases.Operations.InlineAllOf.Response200Body as Response200Body
import qualified TestCases.Operations.InlineAllOf.Response400Body as Response400Body
import qualified TestCases.Operations.InlineAllOf.XSampleAllOfParam as XSampleAllOfParam

operation ::
  H.Operation
    FA.JSONDecodingError
    H.NoPathParams
    QueryParams
    HeaderParams
    RequestBody.RequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestQuerySchema = queryParamsSchema
    , H.requestHeaderSchema = headerParamsSchema
    , H.requestBodySchema = H.requestBody FA.JSON RequestBody.requestBodySchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.post $
    R.make H.NoPathParams
      /- "test-cases"
      /- "inline-all-of"

data QueryParams = QueryParams
  { inlineAllofParam :: InlineAllofParam.InlineAllofParam
  }
  deriving (Eq, Show)

queryParamsSchema :: P.QuerySchema p => p QueryParams QueryParams
queryParamsSchema =
  P.makeParams QueryParams
    ?+ P.required inlineAllofParam InlineAllofParam.paramDef

data HeaderParams = HeaderParams
  { xSampleAllOfParam :: XSampleAllOfParam.XSampleAllOfParam
  }
  deriving (Eq, Show)

headerParamsSchema :: P.HeaderSchema p => p HeaderParams HeaderParams
headerParamsSchema =
  P.makeParams HeaderParams
    ?+ P.required xSampleAllOfParam XSampleAllOfParam.paramDef

data Responses
  = Response200 Response200Body.Response200Body
  | Response400 Response400Body.Response400Body
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON Response200Body.response200BodySchema))
  , (H.Status 400, fmap Response400 (H.responseBody FA.JSON Response400Body.response400BodySchema))
  ]