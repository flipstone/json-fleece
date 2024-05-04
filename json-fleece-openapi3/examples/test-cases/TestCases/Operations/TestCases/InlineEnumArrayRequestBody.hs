{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.InlineEnumArrayRequestBody
  ( operation
  , route
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show, fmap)
import qualified TestCases.Operations.TestCases.InlineEnumArrayRequestBody.RequestBodyItem as RequestBodyItem

operation ::
  H.Operation
    FA.JSONDecodingError
    H.NoPathParams
    H.NoQueryParams
    H.NoHeaderParams
    [RequestBodyItem.RequestBodyItem]
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestBodySchema = H.requestBody FA.JSON (FC.list RequestBodyItem.requestBodyItemSchema)
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.post $
    R.make H.NoPathParams
      /- "test-cases"
      /- "inline-enum-array-request-body"

data Responses
  = Response201 H.NoResponseBody
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 201, fmap Response201 (H.noResponseBody))
  ]