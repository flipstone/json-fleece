{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.NonEmptyTextPathParamRef
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
import qualified TestCases.Operations.NonEmptyTextPathParamRef.NonEmptyParam as NonEmptyTextType
import qualified TestCases.Types.NonEmptyTextType as NonEmptyTextType

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
  { nonEmptyParam :: NonEmptyTextType.NonEmptyTextType
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /- "non-empty-text-path-param-ref"
      /+ R.Param NonEmptyTextType.paramDef nonEmptyParam

newtype Responses
  = Response204 H.NoResponseBody
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 204, fmap Response204 (H.noResponseBody))
  ]