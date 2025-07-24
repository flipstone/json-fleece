{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.HeaderParams
  ( operation
  , route
  , HeaderParams(..)
  , headerParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import qualified Beeline.HTTP.Client as H
import Beeline.Params ((?+))
import qualified Beeline.Params as P
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Maybe, Show, fmap)
import qualified TestCases.Operations.TestCases.HeaderParams.BooleanParam as BooleanParam
import qualified TestCases.Operations.TestCases.HeaderParams.InlineEnumIntParam as InlineEnumIntParam
import qualified TestCases.Operations.TestCases.HeaderParams.InlineEnumParam as InlineEnumParam
import qualified TestCases.Operations.TestCases.HeaderParams.StringParam as StringParam
import qualified TestCases.Types.FieldTestCases as FieldTestCases

operation ::
  H.Operation
    FA.JSONDecodingError
    H.NoPathParams
    H.NoQueryParams
    HeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestHeaderSchema = headerParamsSchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.get $
    R.make H.NoPathParams
      /- "test-cases"
      /- "header-params"

data HeaderParams = HeaderParams
  { booleanParam :: BooleanParam.BooleanParam
  , inlineEnumIntParam :: Maybe InlineEnumIntParam.InlineEnumIntParam
  , inlineEnumParam :: Maybe InlineEnumParam.InlineEnumParam
  , stringParam :: StringParam.StringParam
  }
  deriving (Eq, Show)

headerParamsSchema :: P.HeaderSchema p => p HeaderParams HeaderParams
headerParamsSchema =
  P.makeParams HeaderParams
    ?+ P.required booleanParam BooleanParam.paramDef
    ?+ P.optional inlineEnumIntParam InlineEnumIntParam.paramDef
    ?+ P.optional inlineEnumParam InlineEnumParam.paramDef
    ?+ P.required stringParam StringParam.paramDef

data Responses
  = Response200 FieldTestCases.FieldTestCases
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON FieldTestCases.fieldTestCasesSchema))
  ]