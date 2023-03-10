{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.GetMultiplePathsParams
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
import qualified TestCases.Operations.GetMultiplePathsParams.Param1 as Param1
import qualified TestCases.Operations.GetMultiplePathsParams.Param2 as Param2
import qualified TestCases.Types.FieldTestCases as FieldTestCases

operation ::
  H.Operation
    H.ContentTypeDecodingError
    PathParams
    H.NoQueryParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.responseSchemas = responseSchemas
    }

data PathParams = PathParams
  { param1 :: Param1.Param1
  , param2 :: Param2.Param2
  }
  deriving (Eq, Show)

route :: R.Router r => r PathParams
route =
  R.get $
    R.make PathParams
      /- "test-cases"
      /+ R.Param Param1.paramDef param1
      /- "multiple-path-params"
      /+ R.Param Param2.paramDef param2

data Responses
  = Response200 FieldTestCases.FieldTestCases
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema H.ContentTypeDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON FieldTestCases.fieldTestCasesSchema))
  ]