{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.MultipleResponsesCodes
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
import qualified TestCases.Types.FieldTestCases as FieldTestCases

operation ::
  H.Operation
    H.ContentTypeDecodingError
    H.NoPathParams
    H.NoQueryParams
    FieldTestCases.FieldTestCases
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestBodySchema = H.requestBody FA.JSON FieldTestCases.fieldTestCasesSchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.post $
    R.make H.NoPathParams
      /- "test-cases"
      /- "multiple-responses-codes"

data Responses
  = Response201 H.NoResponseBody
  | Response422 H.NoResponseBody
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 201, fmap Response201 (H.noResponseBody))
  , (H.Status 422, fmap Response422 (H.noResponseBody))
  ]