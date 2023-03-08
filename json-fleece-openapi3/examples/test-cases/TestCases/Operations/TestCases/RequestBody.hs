{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.RequestBody
  ( operation
  , PathParams(..)
  , route
  , QueryParams(..)
  , queryParamsSchema
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Show)
import qualified TestCases.Types.FieldTestCases as FieldTestCases

operation ::
  H.Operation
    H.ContentTypeDecodingError
    PathParams
    QueryParams
    FieldTestCases.FieldTestCases
    H.NoResponseBody
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestQuerySchema = queryParamsSchema
    , H.requestBodySchema = H.requestBody FA.JSON FieldTestCases.fieldTestCasesSchema
    }

data PathParams = PathParams
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.post $
    R.make PathParams
      /- "test-cases"
      /- "request-body"

data QueryParams = QueryParams
  deriving (Eq, Show)

queryParamsSchema :: H.QuerySchema q => q QueryParams QueryParams
queryParamsSchema =
  H.makeQuery QueryParams