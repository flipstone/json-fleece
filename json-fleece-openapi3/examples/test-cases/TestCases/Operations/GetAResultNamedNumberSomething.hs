{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetAResultNamedNumberSomething
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
import qualified TestCases.Types.Num2SchemaStartingWithNumber as Num2SchemaStartingWithNumber

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
      /- "op-with-number-prefixed-type"

data Responses
  = Response200 Num2SchemaStartingWithNumber.Num2SchemaStartingWithNumber
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON Num2SchemaStartingWithNumber.num2SchemaStartingWithNumberSchema))
  ]