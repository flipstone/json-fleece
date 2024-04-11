{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.ParamRef
  ( operation
  , route
  , HeaderParams(..)
  , headerParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show, fmap)
import qualified TestCases.Operations.TestCases.ParamRef.XSampleHeaderParam as XSampleHeaderParam

operation ::
  H.Operation
    H.ContentTypeDecodingError
    H.NoPathParams
    H.NoQueryParams
    HeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestHeaderSchema = headerParamsSchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.get $
    R.make H.NoPathParams
      /- "test-cases"
      /- "param-ref"

data HeaderParams = HeaderParams
  { xSampleHeaderParam :: XSampleHeaderParam.XSampleHeaderParam
  }
  deriving (Eq, Show)

headerParamsSchema :: H.ParameterCollectionSchema p => p HeaderParams HeaderParams
headerParamsSchema =
  H.makeParams HeaderParams
    ?+ H.required xSampleHeaderParam XSampleHeaderParam.paramDef

data Responses
  = Response204 H.NoResponseBody
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 204, fmap Response204 (H.noResponseBody))
  ]