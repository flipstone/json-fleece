{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.RequestBody
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
      /- "request-body"

data Responses
  = Response201 FieldTestCases.FieldTestCases
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 201, fmap Response201 (H.responseBody FA.JSON FieldTestCases.fieldTestCasesSchema))
  ]