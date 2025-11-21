{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.ReferenceResponses
  ( operation
  , route
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Show, fmap)
import qualified TestCases.Operations.TestCases.ReferenceResponses.Response500Body as Response500Body
import qualified TestCases.Types.BadRequestError as BadRequestError

operation ::
  H.Operation
    FA.JSONDecodingError
    H.NoPathParams
    H.NoQueryParams
    H.NoHeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.get $
    R.make H.NoPathParams
      /- "test-cases"
      /- "reference-responses"

data Responses
  = Response201 H.NoResponseBody
  | Response400 BadRequestError.BadRequestError
  | Response404 H.NoResponseBody
  | Response500 Response500Body.Response500Body
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 201, fmap Response201 (H.noResponseBody))
  , (H.Status 400, fmap Response400 (H.responseBody FA.JSON BadRequestError.badRequestErrorSchema))
  , (H.Status 404, fmap Response404 (H.noResponseBody))
  , (H.Status 500, fmap Response500 (H.responseBody FA.JSON Response500Body.response500BodySchema))
  ]