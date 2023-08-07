{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.InlineObjectStringResponse
  ( operation
  , route
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Aeson.Beeline as FA
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show, fmap)

operation ::
  H.Operation
    H.ContentTypeDecodingError
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
      /- "inline-object-string-response"

data Responses
  = Response200 (Map.Map T.Text T.Text)
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON (FC.jsonMap FC.text)))
  ]
