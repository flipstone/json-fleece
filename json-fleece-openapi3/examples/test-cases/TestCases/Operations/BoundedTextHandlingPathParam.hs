{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.BoundedTextHandlingPathParam
  ( operation
  , PathParams(..)
  , route
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/+), (/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Show, fmap)
import qualified TestCases.Operations.BoundedTextHandlingPathParam.BoundedHandlingParam as BoundedTextType
import qualified TestCases.Types.BoundedTextType as BoundedTextType

operation ::
  H.Operation
    FA.JSONDecodingError
    PathParams
    H.NoQueryParams
    H.NoHeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.responseSchemas = responseSchemas
    }

newtype PathParams = PathParams
  { boundedHandlingParam :: BoundedTextType.BoundedTextType
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /- "bounded-text-handling-path-param"
      /+ R.Param BoundedTextType.paramDef boundedHandlingParam

newtype Responses
  = Response204 H.NoResponseBody
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 204, fmap Response204 (H.noResponseBody))
  ]