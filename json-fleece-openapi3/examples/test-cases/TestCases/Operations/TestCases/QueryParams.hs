{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCases.Operations.TestCases.QueryParams
  ( operation
  , route
  , QueryParams(..)
  , queryParamsSchema
  , Responses(..)
  , responseSchemas
  ) where

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as H
import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Aeson.Beeline as FA
import Prelude (($), Eq, Maybe, Show, fmap)
import qualified TestCases.Operations.TestCases.QueryParams.BooleanParam as BooleanParam
import qualified TestCases.Operations.TestCases.QueryParams.InlineEnumIntParam as InlineEnumIntParam
import qualified TestCases.Operations.TestCases.QueryParams.InlineEnumParam as InlineEnumParam
import qualified TestCases.Operations.TestCases.QueryParams.OptionalArrayParam as OptionalArrayParam
import qualified TestCases.Operations.TestCases.QueryParams.RequiredArrayParam as RequiredArrayParam
import qualified TestCases.Operations.TestCases.QueryParams.StringParam as StringParam
import qualified TestCases.Types.FieldTestCases as FieldTestCases

operation ::
  H.Operation
    FA.JSONDecodingError
    H.NoPathParams
    QueryParams
    H.NoHeaderParams
    H.NoRequestBody
    Responses
operation =
  H.defaultOperation
    { H.requestRoute = route
    , H.requestQuerySchema = queryParamsSchema
    , H.responseSchemas = responseSchemas
    }

route :: R.Router r => r H.NoPathParams
route =
  R.get $
    R.make H.NoPathParams
      /- "test-cases"
      /- "query-params"

data QueryParams = QueryParams
  { booleanParam :: BooleanParam.BooleanParam
  , inlineEnumIntParam :: Maybe InlineEnumIntParam.InlineEnumIntParam
  , inlineEnumParam :: Maybe InlineEnumParam.InlineEnumParam
  , optionalArrayParam :: [OptionalArrayParam.OptionalArrayParam]
  , requiredArrayParam :: NEL.NonEmpty RequiredArrayParam.RequiredArrayParam
  , stringParam :: StringParam.StringParam
  }
  deriving (Eq, Show)

queryParamsSchema :: H.ParameterCollectionSchema p => p QueryParams QueryParams
queryParamsSchema =
  H.makeParams QueryParams
    ?+ H.required booleanParam BooleanParam.paramDef
    ?+ H.optional inlineEnumIntParam InlineEnumIntParam.paramDef
    ?+ H.optional inlineEnumParam InlineEnumParam.paramDef
    ?+ H.explodedArray optionalArrayParam OptionalArrayParam.paramDef
    ?+ H.explodedNonEmpty requiredArrayParam RequiredArrayParam.paramDef
    ?+ H.required stringParam StringParam.paramDef

data Responses
  = Response200 FieldTestCases.FieldTestCases
  deriving (Eq, Show)

responseSchemas :: [(H.StatusRange, H.ResponseBodySchema FA.JSONDecodingError Responses)]
responseSchemas =
  [ (H.Status 200, fmap Response200 (H.responseBody FA.JSON FieldTestCases.fieldTestCasesSchema))
  ]